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


import ru.biocad.algo.alignment.AlignmentResult
import ru.biocad.algo.alignment.internal.Alignment
import ru.biocad.ds.matrix.internal.DimensionType
import ru.biocad.ds.trie._
import spire.implicits._

import scala.collection.mutable.{ArrayBuffer => MArrayBuffer, Stack => MStack}
import scala.language.existentials


trait YCatOps[Symbol, SequenceLabel] {

  type TrieDataSet = TrieData[Symbol, _ <: MArrayBuffer[Int]]

  case class PreparationResult(trie : ITrie[Symbol, _ <: MArrayBuffer[Int]],
                               edge : MArrayBuffer[Symbol],
                               forkStack : MStack[Int],
                               currentPath : MArrayBuffer[TrieDataSet])

  protected val _trie = new Trie[Symbol, MArrayBuffer[Int]](terminal)
  private [this] val _labels = new java.util.HashMap[Int, SequenceLabel]()
  val label2ann = new java.util.HashMap[Int, Int => String => Any]()

  def terminal: Symbol
  def encodeLabel(label: SequenceLabel): Int = label.hashCode()
  def sequenceAnnotation(encodedLabel: Int): SequenceLabel = _labels.get(encodedLabel)

  def push(s : IndexedSeq[Symbol], label : SequenceLabel, annotation : Int => String => Any) : Boolean = {
    val encodedLabel = encodeLabel(label)

    if (_labels.containsKey(encodedLabel)) {
      false
    }
    else {
      // Add sequence to the reference trie
      val terminate: TrieData[Symbol, MArrayBuffer[Int]] = _trie.push(s)

      // Get path from the root node to the terminate node
      // .tail because we want drop last terminal symbol (which usually is '$')
      val path: IndexedSeq[TrieData[Symbol, MArrayBuffer[Int]]] = _trie.toRoot(terminate).toIndexedSeq.reverse.tail

      // Add annotation coordinates into each node in the path
      cfor(0)(_ < path.length, _ + 1) { i =>
        if (path(i).data == null) path(i).data = MArrayBuffer(encodedLabel)
        else                      path(i).data += encodedLabel
      }

      // Memoize the hash of the label and the label itself
      _labels.put(encodedLabel, label)
      label2ann.put(encodedLabel, annotation)

      // Put the label at the terminal node to retrieve it later
      terminate.terminal = encodedLabel

      pushLogic(s, path)

      cachedTrieAlias = null

      true
    }
  }

  protected def pushLogic(s: IndexedSeq[Symbol], path: IndexedSeq[TrieData[Symbol, MArrayBuffer[Int]]]): Unit = {}

  protected def prepareForTraversing(trie: Trie[Symbol, MArrayBuffer[Int]],
                                     traits: Query[Symbol, SequenceLabel],
                                     constrain: Boolean,
                                     useCache: Boolean = false) : PreparationResult = {

    // check if conditions same as previous, if not then recompute cache
    if (prevTraits != traits) {
      cachedTrieAlias = null
      prevTraits = traits
    }

    val trieAlias = useCache match {
      case false => getTrieAlias(trie, traits, constrain)
      case true => getCachedTrieAlias(trie, traits, constrain)
    }

    val edge = MArrayBuffer.empty[Symbol]
    val forkStack = new MStack[Int]()
    val currentPath = MArrayBuffer.empty[TrieDataSet]
    forkStack.push(trieAlias.root.id)

    PreparationResult(trieAlias, edge, forkStack, currentPath)
  }

  @volatile private[this] var prevTraits: Query[Symbol, SequenceLabel] = null
  @volatile private[this] var cachedTrieAlias: ITrie[Symbol, _ <: MArrayBuffer[Int]] = null
  private[this] val lock = new AnyRef

  private[this] def getCachedTrieAlias(trie: Trie[Symbol, MArrayBuffer[Int]],
                                       traits: Query[Symbol, SequenceLabel],
                                       constrain: Boolean): ITrie[Symbol, _ <: MArrayBuffer[Int]] = {
    var tmp = cachedTrieAlias
    if (tmp == null) {
      lock.synchronized {
        tmp = cachedTrieAlias
        if (tmp == null) {
          tmp = getTrieAlias(trie, traits, constrain = true)
          tmp.depth // reevaluate depth via [[ru.biocad.ds.trie.CachedDepth.depth]] or first thread here will take depth = 0
          cachedTrieAlias = tmp
        }
      }
    }
    tmp
  }

  protected def getTrieAlias(trie: Trie[Symbol, MArrayBuffer[Int]],
                             traits: Query[Symbol, SequenceLabel],
                             constrain: Boolean): ITrie[Symbol, _ <: MArrayBuffer[Int]] = {
    constrain match {
      case true =>
        val labels = traits.labels.map(encodeLabel)
        val constraints = (td : TrieDataSet) =>
          traits.labels.isEmpty || td.parentId.isEmpty || td.data.exists(ann => labels.contains(ann))
        new ConstrainedTrieAlias(trie)(constraints)
      case false =>
        new TrieAlias(trie)
    }
  }

  protected def updateState(previousNode: TrieDataSet,
                            currentNode: TrieDataSet,
                            edgeBuffer: MArrayBuffer[Symbol],
                            forkStack: MStack[Int],
                            currentPath: MArrayBuffer[TrieDataSet],
                            aligner: Alignment[Symbol],
                            alignerDimension: DimensionType) : Unit = {
    if (previousNode.isLeaf) {
      updateFork(currentNode, forkStack, aligner, alignerDimension)
      updatePath(currentNode, currentPath)
    }

    updateEdge(currentNode, edgeBuffer)

    if (currentNode.isFork || currentNode.isLeaf)
      clearEdge(currentNode, forkStack, edgeBuffer, aligner, alignerDimension)

    currentPath += currentNode
  }

  private[this] def updateFork(node: TrieDataSet,
                               forkStack: MStack[Int],
                               aligner: Alignment[Symbol],
                               alignerDimension: DimensionType) : Unit = {
    val parentId = node.parentId.getOrElse(0)
    while (forkStack.top != parentId) {
      forkStack.pop()
      aligner.pop(alignerDimension)
    }
  }

  private[this] def updatePath(node: TrieDataSet,
                               pathBuffer: MArrayBuffer[TrieDataSet]) : Unit = {
    val parentId = node.parentId.getOrElse(0)
    while (pathBuffer.nonEmpty && pathBuffer.last.id != parentId) {
      pathBuffer.trimEnd(1)
    }
  }

  private[this] def updateEdge(currentNode: TrieDataSet,
                               edgeBuffer: MArrayBuffer[Symbol]) : Unit =
    currentNode.symbol match {
      case Some(symbol) =>
        if (symbol != terminal) {
          edgeBuffer += symbol
        }
      case None =>
        throw new RuntimeException(s"Node in right trie ${currentNode.id} does not contain any symbol")
    }

  private[this] def clearEdge(currentNode: TrieDataSet,
                              forkStack: MStack[Int],
                              edgeBuffer: MArrayBuffer[Symbol],
                              aligner: Alignment[Symbol],
                              alignerDimension: DimensionType) : Unit = {
    forkStack.push(currentNode.id)
    aligner.push(edgeBuffer)(alignerDimension)
    edgeBuffer.clear()
  }

  protected def maybeCallback(aligner: Alignment[Symbol],
                              condition: AlignmentResult[Symbol] => Boolean,
                              leftCurrentPath: MArrayBuffer[TrieDataSet],
                              rightCurrentPath: MArrayBuffer[TrieDataSet],
                              callback: QueryResult[Symbol] => Unit) : Unit = {
    val alignment = aligner.traceback()
    if (condition(alignment)) {
      val req = rightCurrentPath.map(x => x.symbol.get).init // Drop last terminal symbol. Can throw an error on empty case
      val ref = restoreAnnotations(leftCurrentPath)
      callback(new QueryResult[Symbol](req, alignment, ref))
    }
  }

  protected def restoreAnnotations(currentPath: MArrayBuffer[TrieDataSet]) = {
    val encodedLabel: Int = currentPath.last.terminal
    val annotationFn = label2ann.get(encodedLabel)

    val annPath = currentPath.zipWithIndex.map {
      case (node, idx) => node.data.map { label =>
        val annotation = annotationFn(idx)
        YCatAnnotation(label, annotation)
      }
    }

    annPath
  }

  protected def skipStates(previousStateIterator: Iterator[TrieDataSet],
                           currentStateIterator: Iterator[TrieDataSet]) : (Iterator[TrieDataSet], Iterator[TrieDataSet]) = {
    var loop = true
    while (loop) {
      val curr = {
        previousStateIterator.next()
        currentStateIterator.next()
      }
      if (curr.isLeaf) loop = false
    }
    (previousStateIterator, currentStateIterator)
  }
}
