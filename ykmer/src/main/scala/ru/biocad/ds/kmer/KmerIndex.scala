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

package ru.biocad.ds.kmer

import ru.biocad.ds.kmer.internal.Hasher

import scala.collection.mutable

/**
 * User: pavel
 * Date: 14.07.14
 * Time: 16:15
 */

class KmerIndex[Symbol, @specialized(Int) Data](hasher : Hasher[Symbol])(implicit val ord : Ordering[Symbol]) {
  private [this] val _hashmap = new mutable.HashMap[Long, mutable.Set[Data]] with mutable.MultiMap[Long, Data]

  def addSeq(s : IndexedSeq[Symbol], data : Iterator[Data]) : Unit =
    (hasher.iterator(s) zip data).foreach {
      case (hashes, d) =>
        hashes.foreach(_hashmap.addBinding(_, d))
    }

  def update(key : IndexedSeq[Symbol], value : Data) : Unit =
    hash(key).next().foreach(ikey => _hashmap.addBinding(ikey, value))

  def apply(key : IndexedSeq[Symbol]) : mutable.HashSet[Data] =
    hash(key).next().flatMap(_hashmap.get).flatten

  def dataIterator(sequence : IndexedSeq[Symbol]) : Iterator[mutable.HashSet[Data]] =
    hash(sequence).map(_.flatMap(_hashmap.get).flatten)

  def getHasher : Hasher[Symbol] = hasher

  private [this] def hash(key : IndexedSeq[Symbol]) : Iterator[mutable.HashSet[Long]] =
    hasher.iterator(key)
}
