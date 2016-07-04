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
import ru.biocad.common.SeqTools._

/**
 * User: pavel
 * Date: 02.10.14
 * Time: 17:40
 */
class SeqToolsTest extends FlatSpec with Matchers {
  "SeqTools" should "truncate" in {
    "name.surename.something".to[IndexedSeq].truncateAfterLast('.').mkString should be ("name.surename")
    "name.surename.something".to[IndexedSeq].suffixAfterLast('.').mkString should be ("something")
  }

  it should "find common prefixes" in {
    SeqTools.commonPrefix(List("hello world", "hello w123", "hellogoodbye", "hell123", "hello world")) should be ("hell")

    SeqTools.longestCommonSubstring(
      "qwertyuiopasdfghjkl",
      "qwasvbrtyuiopasdnhg") should be (Some(LongestCommonResult(3, 6, 10, "rtyuiopasd")))
    SeqTools.longestCommonSubstring(
      "ATGCAAAAAAATTTTTTTTCCCCCCC",
      "TAATTGCAAAAAAATTTTTTACACACCCAA") should be (Some(LongestCommonResult(1, 4, 16, "TGCAAAAAAATTTTTT")))
  }
}
