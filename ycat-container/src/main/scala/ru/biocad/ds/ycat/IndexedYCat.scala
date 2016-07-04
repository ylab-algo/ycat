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

import ru.biocad.ds.kmer.KmerIndex
import ru.biocad.ds.kmer.internal.Hasher
import ru.biocad.ds.trie.{TrieAlias, TrieData}

import scala.collection.{mutable, immutable}

/**
 * User: pavel
 * Date: 31.07.15
 * Time: 15:15
 */
class IndexedYCat[Symbol, SequenceLabel]
                 (terminal : Symbol, stripSimilarity: Boolean = false)
                 (k : Int, alphabet : IndexedSeq[Symbol], wildcards : Map[Symbol, Set[Symbol]])
                 (implicit override val ord : Ordering[Symbol]) extends YCat[Symbol, SequenceLabel](terminal, stripSimilarity)(ord) {

  protected val _kindex = new KmerIndex[Symbol, Int](new Hasher[Symbol](k, alphabet, immutable.TreeMap(wildcards.toSeq:_*)))

  override  protected def pushLogic(s : IndexedSeq[Symbol], path : IndexedSeq[TrieData[Symbol, mutable.ArrayBuffer[Int]]]) : Unit =
    _kindex.addSeq(s, path.drop(k - 1).dropRight(1).map(_.id).iterator)

  def search(sequence : IndexedSeq[Symbol])
            (callback : SearchResult[Symbol] => Unit) : Unit = {
    val coloredTrie = new TrieAlias[Symbol, Boolean](_trie)
    val maxDepth = sequence.size - k + 1

    _kindex
      .dataIterator(sequence)
      .foldLeft(null.asInstanceOf[mutable.HashSet[Int]]) {
      case (_, nodes) =>
        nodes.foreach(node => coloredTrie(node).data = true)
        nodes
    }
      .flatMap {
      case node =>
        Option(coloredTrie.toRoot(coloredTrie(node)))
          .map(_.take(sequence.size).toIndexedSeq.reverse)
          .filter(_.takeRight(maxDepth).forall(_.data))
    }
      .foreach {
      case path =>
        callback(SearchResult(path.flatMap(_.symbol), path.map(td => _trie(td.id))))
    }
  }

}
