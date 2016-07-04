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

package ru.biocad.ds.trie

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: pavel
 * Date: 14.07.14
 * Time: 12:46
 */
class TrieTest extends FlatSpec with Matchers {
  val terminate = '$'
  val t = new Trie[Char, Int](terminate)

  val r = new Random()
  val lst = Range(0, 20).map(_ => Range(0, 10).map(_ => ('A' + r.nextInt(4)).toChar).mkString).toArray

  lst.zipWithIndex.foreach(tpl => {
    val (s, i) = tpl
    val n = t.push(s)
    n.data = i
  })

  "Trie" should "store nodes" in {
    t.forall(n => n.id == t(n.id).id) shouldBe true
  }

  it should "iterate" in {
    val s = ArrayBuffer.empty[TrieData[Char, Int]]
    var old : TrieData[Char, Int] = t(0)
    t.foreach(cur => {
      if (cur.isLeaf) {
        s.flatMap(_.symbol).mkString shouldBe lst(cur.data)
      }
      else {
        val check = old.isLeaf
        if (check) {
          while (s.nonEmpty && !cur.parentId.contains(s.last.id)) {
            s.remove(s.size - 1)
          }
        }
        s += cur
      }
      old = cur
    })
  }

  it should "iterate back" in {
    val forward = t.flatMap(node => if (node.isLeaf) Some(node.data) else None)
    val reverse = t.reverseIterator.flatMap(node => if (node.isLeaf) Some(node.data) else None).toSeq

    forward should contain theSameElementsInOrderAs reverse.reverse
  }

  it should "go to root" in {
    val path = t.takeWhile(!_.isLeaf).toArray
    t.toRoot(path.last).toArray should be (path.reverse)
  }
}
