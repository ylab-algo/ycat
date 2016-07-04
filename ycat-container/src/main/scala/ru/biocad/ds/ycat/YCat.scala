/*
 * Copyright (c) 2014, BIOCAD Bioinformatics Group and co-authors
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. All advertising materials mentioning features or use of this software
 *     must display the following acknowledgement:
 *     This product includes software developed by the organization.
 *  4. Neither the name of the organization nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL BIOCAD BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ru.biocad.ds.ycat

import ru.biocad.algo.alignment.{AlignmentFactory, AlignmentResult}
import ru.biocad.ds.matrix.internal.DimensionType
import ru.biocad.ds.trie._

import scala.collection.mutable.{ArrayBuffer => MArrayBuffer, Map => MMap, PriorityQueue => MPriorityQueue}
import scala.language.existentials


/**
 * User : pavel
 * Date : 07.08.14
 * Time : 10:20
 */
class YCat[Symbol, SequenceLabel](override val terminal : Symbol, stripSimilarity : Boolean = false)
                                 (implicit val ord : Ordering[Symbol]) extends YCatOps[Symbol, SequenceLabel] {

  private[this] val similarity: AlignmentResult[Symbol] => Double = alignmentResult => stripSimilarity match {
    case true => alignmentResult.stripSimilarity
    case false => alignmentResult.similarity
  }

  implicit private[this] val pqOrd: Ordering[QueryResult[Symbol]] = Ordering.by(qr => -qr.score)

  def annotate(refsCount : Int, threshold : Double)
              (sequence : IndexedSeq[Symbol], traits : Query[Symbol, SequenceLabel]) : Option[AnnotationResult[Symbol, SequenceLabel]] = {
    require(refsCount > 0, "Number of references must be greater than zero")

    val priorityQueue = MPriorityQueue.empty[QueryResult[Symbol]]

    def condition(alignment: AlignmentResult[Symbol]): Boolean = {
      similarity(alignment) >= threshold && (priorityQueue.size < refsCount || alignment.score > priorityQueue.head.score)
    }

    def callback(result: QueryResult[Symbol]): Unit = {
      priorityQueue += result
      if (priorityQueue.size == refsCount) {
        priorityQueue.dequeue()
      }
    }

    query(sequence, traits)(callback, condition)
    val results: IndexedSeq[QueryResult[Symbol]] = priorityQueue.dequeueAll

    results match {
      case res if res.nonEmpty => Some(new AnnotationResult[Symbol, SequenceLabel](res, similarity))
      case _ => None
    }
  }

  def query(sequence : IndexedSeq[Symbol], traits : Query[Symbol, SequenceLabel])
           (callback : QueryResult[Symbol] => Unit, condition : AlignmentResult[Symbol] => Boolean = _ => true) : Unit = {

    val alias = prepareForTraversing(_trie, traits, constrain = true, useCache = true)

    val aligner = AlignmentFactory.create[Symbol](traits.algorithm)(sequence.size + 1, alias.trie.depth)(traits.substitution, traits.gap)

    aligner.push(sequence)(DimensionType.Horizontal)

    (alias.trie.iterator zip alias.trie.tail.iterator).foreach {
      case (previousNode, currentNode) =>
        updateState(previousNode,
          currentNode,
          alias.edge,
          alias.forkStack,
          alias.currentPath,
          aligner,
          DimensionType.Vertical)

        if (currentNode.isLeaf) {
          val alignment = aligner.traceback()
          if (condition(alignment)) {
            val annPath = restoreAnnotations(alias.currentPath)
            callback(new QueryResult[Symbol](sequence, alignment, annPath))
          }
        }
    }
  }

  def annotateTrie(refsCount : Int, threshold : Double)
                  (seqs : Seq[IndexedSeq[Symbol]], traits : Query[Symbol, SequenceLabel]) : Option[Seq[AnnotationResult[Symbol, SequenceLabel]]] = {
    require(refsCount > 0, "Number of references must be greater than zero")

    val results: MMap[IndexedSeq[Symbol], MPriorityQueue[QueryResult[Symbol]]] = MMap()

    val condition : AlignmentResult[Symbol] => Boolean =
      alignment => similarity(alignment) >= threshold

    val callback : QueryResult[Symbol] => Unit = result => {
      val key = result.source
      if (results.contains(key) && (results(key).size < refsCount || result.score > results(key).head.score)) {
        results(key) += result
        if (results(key).size == refsCount) {
          results(key).dequeue()
        }
      } else {
        results(key) = MPriorityQueue[QueryResult[Symbol]](result)
      }
    }

    val rightTrie = new Trie[Symbol, MArrayBuffer[Int]](terminal)
    for (s <- seqs) rightTrie.push(s)

    trie2trie(rightTrie, traits)(callback, condition)

    results match {
      case res if res.nonEmpty =>
        Some(res.map { case (rightTrieSource, pq) =>
          new AnnotationResult[Symbol, SequenceLabel](pq.dequeueAll, similarity)
        }.toSeq)

      case _ =>
        None
    }
  }


  /**
   * Trie to trie alignment that helps to align compressible data.
   * Speed of the alignment depends on data compression rate.
   *
   * @param request   The trie of sequences to annotate
   * @param traits    Traits for constraints of tries
   * @param callback  The function called when the alignment condition is satisfied
   * @param condition The condition to be satisfied by alignment
   * @return
   */
  def trie2trie(request : Trie[Symbol, MArrayBuffer[Int]], traits : Query[Symbol, SequenceLabel])
               (callback : QueryResult[Symbol] => Unit, condition : AlignmentResult[Symbol] => Boolean = _ => true) : Unit = {

    val left = prepareForTraversing(_trie, traits, constrain = true)
    val right = prepareForTraversing(request, traits, constrain = false)

    val aligner = AlignmentFactory.create[Symbol](traits.algorithm)(right.trie.depth, left.trie.depth)(traits.substitution, traits.gap)

    var rightTrieIterationN = 0
    (left.trie.iterator zip left.trie.tail.iterator).foreach {
      case (leftPreviousNode, leftCurrentNode) =>

        updateState(leftPreviousNode,
          leftCurrentNode,
          left.edge,
          left.forkStack,
          left.currentPath,
          aligner,
          DimensionType.Vertical)

        if (leftCurrentNode.isLeaf) {
          // Traverse the request trie

          val (rightTrieIterator, rightTrieIteratorTail) = rightTrieIterationN match {
            case n if n == 0 => (right.trie.iterator, right.trie.tail.iterator)
            case n if (n & 1) == 0 =>
              maybeCallback(aligner, condition, left.currentPath, right.currentPath, callback)
              skipStates(right.trie.iterator, right.trie.tail.iterator)
            case _ =>
              maybeCallback(aligner, condition, left.currentPath, right.currentPath, callback)
              skipStates(right.trie.reverseIterator, right.trie.reverseIterator.drop(1))
          }

          (rightTrieIterator zip rightTrieIteratorTail).foreach {
            case (rightPreviousNode, rightCurrentNode) =>

            updateState(rightPreviousNode,
                rightCurrentNode,
                right.edge,
                right.forkStack,
                right.currentPath,
                aligner,
                DimensionType.Horizontal)

              if (rightCurrentNode.isLeaf)
                maybeCallback(aligner, condition, left.currentPath, right.currentPath, callback)
          }

          rightTrieIterationN += 1
        }
    }
  }
}
