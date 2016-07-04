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

package ru.biocad.ds.kmer.internal

import scala.collection.{immutable, mutable}
import spire.implicits._

/**
 * User: pavel
 * Date: 14.07.14
 * Time: 16:33
 */
class Hasher[Symbol](k : Int, alphabet : IndexedSeq[Symbol], wildcards : immutable.TreeMap[Symbol, Set[Symbol]])
                    (implicit val ord : Ordering[Symbol]) {
  private [this] val _shift = math.ceil(math.log(alphabet.size) / math.log(2.0)).toLong
  private [this] val _mask  = (math.pow(2.0, _shift * k) - 1.0).toLong

  def iterator(s : IndexedSeq[Symbol]) : Iterator[mutable.HashSet[Long]] =
    new Iterator[mutable.HashSet[Long]] {
      private [this] var _position = 0
      private [this] val _last     = mutable.HashSet.empty[Long]

      preStart()

      def hasNext : Boolean = _position < s.size

      def next() : mutable.HashSet[Long] = {
        step()
        _last
      }

      private [this] def preStart() : Unit = cforRange(0 until k - 1){ _ => step() }

      private [this] def step() : Unit = {
        val lastCopy : Array[Long] = if (_last.nonEmpty) _last.toArray else Array(0L)
        _last.clear()

        val symbols = wildcards.getOrElse(s(_position), Set(s(_position)))

        cfor(0)(_ < lastCopy.length, _ + 1){ i =>
          symbols.foreach(symbol => _last += ((lastCopy(i) << _shift) + alphabet.indexOf(symbol)) & _mask)
        }
        _position += 1
      }
    }
}
