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

import scala.util.Random

/**
 * User: pavel
 * Date: 17.07.14
 * Time: 12:24
 */
object Functional {
  def fix[A, B](f : (A => B) => A => B) : A => B =
    f(fix(f))(_)

  def max[T](xs : T*)(implicit ord : Ordering[T]) : T =
    xs.max

  def min[T](xs : T*)(implicit ord : Ordering[T]) : T =
    xs.min

  def avg[T](xs : T*)(implicit num : Numeric[T]) : Double =
    xs.map(num.toDouble(_) / xs.size).sum

  implicit class Countable[A](xs : Iterable[A]) {
    def countMap : Map[A, Int] =
      xs.groupBy(c => c).map {
        case (k, v) => k -> v.size
      }
  }

  implicit class Cartesian[A](xs : Traversable[A]) {
    def cross[B](ys : Traversable[B]) : Traversable[(A, B)] =
      for (i <- xs; j <- ys) yield (i, j)
  }

  implicit class ConditionalElse[T](x : T) {
    def otherIfNot(pred : T => Boolean)(other : => T) : T = otherIf(!pred(_))(other)

    def otherIf(pred : T => Boolean)(other : => T) : T =
      if (pred(x)) other else x
  }

  def time[A](label : String)(a: => A) = {
     val now = System.nanoTime
     val result = a
     val micros = (System.nanoTime - now) / 1000
     println(s"$label: $micros microseconds")
     result
  }

  def mutate(s : Array[Char], alphabet : String, mutations : Int) : Array[Char] = {
    val news = Array.fill[Char](s.length)(0)
    s.copyToArray(news)

    (0 until Random.nextInt(mutations)).foreach {
      case i =>
        news(Random.nextInt(s.length)) = alphabet.charAt(Random.nextInt(alphabet.length))
    }
    news
  }
}
