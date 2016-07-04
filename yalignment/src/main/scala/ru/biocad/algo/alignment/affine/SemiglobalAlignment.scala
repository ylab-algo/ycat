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

package ru.biocad.algo.alignment.affine

import ru.biocad.algo.alignment.internal.MatrixType
import ru.biocad.ds.matrix.Matrix
import ru.biocad.ds.matrix.internal.DimensionType
import spire.implicits._

import scala.collection.generic.Growable

/**
 * @author frolov
 * @since 09.12.14
 */

class SemiglobalAlignment[Symbol](width : Int,
                                  height : Int,
                                  sub : (Symbol, Symbol) => Int,
                                  gapOpen : Int,
                                  gapExt : Int) extends AffineAlignment[Symbol](width, height, sub, gapOpen, gapExt) {

  /**
   * First string and first column for each matrix set 0.0
   */
  protected def initial(mt : MatrixType, dt : DimensionType) : Int => Int = mt match {
    case matrixTypes => (i: Int) => 0
  }

  override protected def prepare(horizontal : Growable[Option[Symbol]], vertical : Growable[Option[Symbol]]) : (Int, (Int, Int)) = {
    val h = matrix(MatrixType.Primary).realHeight - 1
    val w = matrix(MatrixType.Primary).realWidth - 1

    val seqh = sequences(DimensionType.Horizontal)
    val seqv = sequences(DimensionType.Vertical)

    DimensionType.ALL.map(dt => dt -> {
      val (d, foo) = dt match {
        case DimensionType.Horizontal => (w + 1, matrix(MatrixType.Primary)(h, _ : Int))
        case DimensionType.Vertical   => (h + 1, matrix(MatrixType.Primary)(_ : Int, w))
      }
      Iterator.from(0).take(d).map(i => i -> foo(i)).maxBy(_._2)
    }).maxBy(_._2._2) match {
      case (DimensionType.Horizontal, (i, s)) =>
        cforRange(1 to w - i){ k =>
          horizontal += Some(seqh(seqh.size - k))
          vertical += None
        }
        (s, (h, i))
      case (DimensionType.Vertical,   (i, s)) =>
        cforRange(1 to h - i){ k =>
          horizontal += None
          vertical += Some(seqv(seqv.size - k))
        }
        (s, (i, w))
    }
  }

  /**
   * condition for continue cycle process
   * continue until reached (0,0)
   */
  override protected def cycleCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 || j != 0

}