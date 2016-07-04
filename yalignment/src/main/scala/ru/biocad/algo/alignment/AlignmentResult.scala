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

package ru.biocad.algo.alignment

/**
 * User: pavel
 * Date: 16.07.14
 * Time: 10:18
 */
case class AlignmentResult[@specialized(Char) Symbol](score : Double,
                                                      horizontal : IndexedSeq[Option[Symbol]],
                                                      vertical : IndexedSeq[Option[Symbol]],
                                                      horizontalStart : Int = 0,
                                                      verticalStart : Int = 0) {

  lazy val similarity = 1.0 - Metrics.relativeHamming(horizontal, vertical)

  /**
   * In case when ver has ambiguity symbols in the begging and the end
   * stripSimilarity isolate unambiguous parts of sequences (parts in []):
   * ver: fa[sasfgfgasfas]dfsg
   * hor: --[asfasdfasdfa]----
   */
  lazy val stripSimilarity = {
    val (h, v) = AlignmentResult.strip(horizontal, vertical)
    1.0 - Metrics.relativeHamming(h, v)
  }

}


object AlignmentResult {

  /** Strip reference and query sequences based on query sequence longest empty prefix and suffix */
  def strip[T](query: IndexedSeq[Option[T]], reference: IndexedSeq[Option[T]]): (IndexedSeq[Option[T]], IndexedSeq[Option[T]]) = {
      require(query.size == reference.size, "Lengths of the read and the reference should be equal")

      /* Size of the longest sequence of non-empty options */
      val emptyN: Iterator[Option[T]] => Int = iterator => iterator.takeWhile(e => e.isEmpty).size

      val nPrefix: Int = emptyN(query.iterator)
      val nSuffix: Int = emptyN(query.reverseIterator)

      val trimmed = (s: IndexedSeq[Option[T]]) => s.slice(nPrefix, s.size - nSuffix)

      trimmed(query) -> trimmed(reference)
    }

}
