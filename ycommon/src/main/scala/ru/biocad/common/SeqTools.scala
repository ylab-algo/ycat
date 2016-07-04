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

import ru.biocad.common.Functional.Cartesian

/**
 * User: pavel
 * Date: 08.09.14
 * Time: 17:39
 */
object SeqTools {
  case class LongestCommonResult[Symbol](firstStart : Int, secondStart : Int, size : Int, lcs : IndexedSeq[Symbol])

  implicit class YIndexedSeq[T](s : IndexedSeq[T]) {
    def truncateAfterLast(c : T) : IndexedSeq[T] =
      s.zipWithIndex.reverseIterator.find(_._1 == c) match {
        case Some((_, i)) => s.slice(0, i)
        case None         => s
      }

    def suffixAfterLast(c : T) : IndexedSeq[T] =
      s.zipWithIndex.reverseIterator.find(_._1 == c) match {
        case Some((_, i)) => s.slice(i + 1, s.size)
        case None         => IndexedSeq.empty[T]
      }

    def splitByFirst(c : T) : (IndexedSeq[T], IndexedSeq[T]) =
      s.splitAt(s.indexOf(c)) match {
        case (head, last) => (head, last.drop(1))
      }

    def strip(symbols : Set[T]) : IndexedSeq[T] =
      s.dropWhile(symbols.contains).reverse.dropWhile(symbols.contains).reverse

    def prefix(delimiter : Seq[T]) : Seq[T] = s.indexOfSlice(delimiter) match {
      case -1 => s
      case idx : Int => s.take(idx)
    }
  }

  def commonPrefix(s : Iterable[String]) : String =
    s.headOption match {
      case None    => ""
      case Some(x) => s.tail.foldRight(x)((e, acc) => (e zip acc).takeWhile(t => t._1 == t._2).map(_._1).mkString)
    }

  def lexographicalCompare[Symbol](s : Seq[Symbol], t : Seq[Symbol])(implicit ord : Ordering[Symbol]) : Int =
    (s zip t).find {
      case (i, j) =>
        ord.compare(i, j) != 0
    } match {
      case Some((i, j)) => ord.compare(i, j)
      case None => s.size - t.size
    }

  //todo rewrite for O(nlogn)
  def longestCommonSubstring[Symbol](s1 : IndexedSeq[Symbol], s2 : IndexedSeq[Symbol]) : Option[LongestCommonResult[Symbol]] = (s1, s2) match {
    case (s1, s2) if s1.nonEmpty && s2.nonEmpty =>
      val m = Array.ofDim[Int](s1.size + 1, s2.size + 1)

      ((1 to s1.size) cross (1 to s2.size)).foreach { case (i,j) =>
        m(i)(j) = if (s1(i - 1) == s2(j - 1)) m(i - 1)(j - 1) + 1 else 0
      }

      val (end1, end2) = ((1 to s1.size) cross (1 to s2.size)).maxBy { case (i, j) => m(i)(j) }

      m(end1)(end2) match {
        case 0 => None
        case length : Int => Some(LongestCommonResult(end1 - length, end2 - length, length, s1.slice(end1 - length, end1)))
      }
    case _ => None
  }
}
