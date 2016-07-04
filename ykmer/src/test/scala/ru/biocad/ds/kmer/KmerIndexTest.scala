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

import org.scalatest.{FlatSpec, Matchers}
import ru.biocad.ds.kmer.internal.Hasher

import scala.collection.immutable

/**
 * User: pavel
 * Date: 15.07.14
 * Time: 10:33
 */
class KmerIndexTest extends FlatSpec with Matchers {
  private val k = 3
  private val h = new Hasher[Char](k, "ACGT", immutable.TreeMap('N' -> Set('A', 'C', 'G', 'T'),
                                                                'M' -> Set('A', 'C'),
                                                                'W' -> Set('A', 'T')))
  private val kmer = new KmerIndex[Char, String](h)

  init()

  "K-mer Index" should "get simple k-mers" in {
    kmer("AAA") should be (Set("s1", "s2"))
    kmer("AAC") should be (Set("s3", "t1"))
    kmer("ACT") should be (Set("t2"))
    kmer("CTG") should be (Set("t3"))
  }

  it should "get complex k-mers" in {
    kmer("AAN") should be (Set(/* AAA: */"s1", /* AAA: */"s2", /* AAC: */"s3", /* AAC: */"t1"))
    kmer("AMW") should be (Set(/* AAA: */"s1", /* AAA: */"s2", /* ACT: */"t2"))
  }

  def init() : Unit = {
    val s = "AAAAC"
    val t = "AACTG"

    kmer.addSeq(s, s.sliding(k).zipWithIndex.map(tpl => s"s${tpl._2 + 1}"))
    kmer.addSeq(t, t.sliding(k).zipWithIndex.map(tpl => s"t${tpl._2 + 1}"))
  }
}
