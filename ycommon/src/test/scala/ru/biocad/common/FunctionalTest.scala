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

package ru.biocad.common

import org.scalatest.{FlatSpec, Matchers}
import ru.biocad.common.Functional._

/**
 * User: pavel
 * Date: 17.07.14
 * Time: 14:15
 */
class FunctionalTest extends FlatSpec with Matchers {
  "Functional" should "provide fix" in {
    val fib = fix[Unit, Stream[Int]](f => (_ : Unit) => 0 #:: 1 #:: (f() zip f().tail).map(tpl => tpl._1 + tpl._2))()

    fib(10) should be (55)
  }

  it should "provide otherIfNot" in {
    "a".otherIfNot(_ == "a")("b") should be ("a")
    123.otherIfNot(_ % 2 == 0)(122) should be (122)
  }

  it should "provide cross product" in {
    List(1,2) cross "ab" should be (List((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')))
  }

  it should "provide elipsis max" in {
    max(1) should be (1)
    max(1,3) should be (3)
    max(0 to 10:_*) should be (10)
  }
}
