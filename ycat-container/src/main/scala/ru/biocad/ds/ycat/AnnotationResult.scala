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
import ru.biocad.common.Functional._

/**
 * User: pavel
 * Date: 17.10.14
 * Time: 16:12
 */
class AnnotationResult[Symbol, SequenceLabel](queryResults: Iterable[QueryResult[Symbol]],
                                              similarity: AlignmentResult[Symbol] => Double) {
  private[this] val (_source, _variants, _data) = extractData(queryResults)

  val similarities = queryResults.flatMap(qr => qr.labels.map(_ -> similarity(qr.alignment))).toMap
  val scores = queryResults.flatMap(qr => qr.labels.map(_ -> qr.score)).toMap

  def variants = _variants
  def source: IndexedSeq[Symbol] = _source
  def apply(key: String): IndexedSeq[Map[Any, Int]] = _data.map(entry ⇒ entry.map(strings ⇒ strings(key)).countMap)

  private def extractData(results: Iterable[QueryResult[Symbol]]): (IndexedSeq[Symbol], IndexedSeq[Map[Symbol, Int]], IndexedSeq[Iterable[String => Any]]) = {
    assert(results.nonEmpty, "Empty annotation set")
    assert(results.tail.forall(_.source == results.head.source), "QueryResults correspond to different sources")

    val source = results.head.source // sequence that was annotated

    //              ACGTACGT   source
    //
    //            | AGTCGTAC | result1 \
    // IndexedSeq | GTCATTAC | result2  >-- results
    //            | ACGTAGGT | result3 /
    //
    // variants => idx 0                              idx 1
    //             source(idx) = A                    source(idx) = C
    //             variants = Map( A -> 2, G -> 1 )   variants = Map( G -> 1, T -> 1, C -> 1 )
    val annotations = source.indices.map(i => results.map(qr => qr.annotation(i)))
    val variants = annotations.map(a => a.flatMap(sr => sr.target).countMap)
    val data = annotations.map(a ⇒ a.flatMap(sr ⇒ sr.annotation))

    (source, variants, data)
  }
}


case class SymbolResult[Symbol](query: Symbol,
                                target: Option[Symbol] = None,
                                annotation: Iterable[String => Any] = Set.empty)


class QueryResult[Symbol](val source: IndexedSeq[Symbol],
                          val alignment: AlignmentResult[Symbol],
                          path: IndexedSeq[Iterable[YCatAnnotation]]) {
  val score = alignment.score

  lazy val labels = path.last.map(nodeData => nodeData.label).toSet
  lazy val realPath = path.map(nodeData => nodeData.filter(annotation => labels.contains(annotation.label)))

  lazy val annotation: IndexedSeq[SymbolResult[Symbol]] = {
    val targetPath = realPath.drop(alignment.verticalStart).toIterator
    val annotated = (alignment.horizontal zip alignment.vertical).flatMap {
      case (None, tChOp) =>
        if (tChOp.nonEmpty) targetPath.next()
        None
      case (Some(qCh), Some(tCh)) =>
        Some(SymbolResult[Symbol](qCh, Some(tCh), targetPath.next().map(node => node.annotation)))
      case (Some(qCh), None) =>
        Some(SymbolResult[Symbol](qCh))
    }
    // FIXME: Rewrite this to O(n)
    source.take(alignment.horizontalStart).map(SymbolResult[Symbol](_)) ++ annotated ++
      source.takeRight(source.size - annotated.size - alignment.horizontalStart).map(SymbolResult[Symbol](_))
  }
}
