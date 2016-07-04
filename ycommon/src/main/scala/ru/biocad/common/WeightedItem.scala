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

import java.util.Random

/**
 * User: pavel
 * Date: 03.09.14
 * Time: 16:31
 */
case class WeightedItem[T](item : T, weight : Double) extends Ordered[WeightedItem[T]] {
  def compare(that : WeightedItem[T]) : Int =
    weight.compare(that.weight)
}

object WeightedItem {
  def weightedSelection[T](items : Iterable[WeightedItem[T]], r : Random = new Random()) : T = {
    val randomPosition = items.map(_.weight).sum * r.nextDouble()
    val itemsDesc = items.toArray.sortBy(_.weight).reverse
    val cumSum = itemsDesc.scanLeft(0.0) { case (acc, item) => acc + item.weight}.tail

    cumSum.zipWithIndex.find(_._1 > randomPosition).map(_._2) match {
      case Some(idx) if idx < itemsDesc.length => itemsDesc(idx).item
      case _ => throw new RuntimeException("Random selection exception")
    }
  }
}
