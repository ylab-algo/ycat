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

import scala.collection.generic.Growable

/**
 * @author frolov
 * @since 19.12.14
 */
class GlobalAlignment[Symbol](width : Int,
                              height : Int,
                              sub : (Symbol, Symbol) => Int,
                              gapOpen : Int,
                              gapExt : Int) extends AffineAlignment[Symbol](width, height, sub, gapOpen, gapExt) {

  /**
   * First string and first column for
   *
   * `Primary matrix`:
   *
   *                  0       i = j = 0
   *    s(i,j) =
   *              -infinity   i > 0, j > 0
   *
   *
   *
   * `Horizontal matrix`:
   *
   *                    gapOpen         i = j =0
   *    s(i,j) = gapOpen + i*gapExt     i > 0, j = 0
   *                   -infinity        i = 0, j > 0
   *
   *
   *
   * `Vertical matrix`:
   *
   *                   gapOpen          i = j = 0
   *    s(i,j) =      -infinity         i > 0, j = 0
   *              gapOpen + j*gapExt    i = 0, j > 0
   *
   *
   *
   * `Substitution matrix`:
   *
   *    s(i,j) =    -infinity     i, j
   */
  override protected def initial(mt : MatrixType, dt : DimensionType) : Int => Int = mt match {
    case MatrixType.Primary => dt match {
      case _ => (i : Int) => if (i == 0) 0 else gapOpen + (width + height) * gapExt
    }
    case MatrixType.Horizontal => dt match {
      case DimensionType.Horizontal => (i : Int) => gapOpen + i * gapExt
      case DimensionType.Vertical => (i : Int) => if (i == 0) gapOpen else gapOpen + (width + height) * gapExt
    }
    case MatrixType.Vertical => dt match {
      case DimensionType.Horizontal => (i : Int) => if (i == 0) gapOpen else gapOpen + (width + height) * gapExt
      case DimensionType.Vertical => (i : Int) => gapOpen + i * gapExt
    }
    case MatrixType.Substitution => dt match {
      case _ => _ => gapOpen + (width + height) * gapExt
    }
  }

  /**
   * @param horizontal - horizontal data vector
   * @param vertical - vertical data vector
   * @return - (score, (i, j))
   */
  override protected def prepare(horizontal : Growable[Option[Symbol]], vertical : Growable[Option[Symbol]]) : (Int, (Int, Int)) = {
    val i = matrix(MatrixType.Primary).realHeight - 1
    val j = matrix(MatrixType.Primary).realWidth - 1
    (matrix(MatrixType.Primary)(i, j), (i, j))
  }

  /**
   * condition for continue cycle process
   * continue until reached (0,0)
   */
  override protected def cycleCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 || j != 0
}