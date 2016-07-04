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
import ru.biocad.common.Functional._
import ru.biocad.ds.matrix.Matrix
import ru.biocad.ds.matrix.internal.DimensionType
import spire.implicits._

import scala.collection.generic.Growable

/**
 * @author frolov
 * @since 09.12.14
 */

class LocalAlignment[Symbol](width : Int,
                             height : Int,
                             sub : (Symbol, Symbol) => Int,
                             gapOpen : Int,
                             gapExt : Int) extends AffineAlignment[Symbol](width, height, sub, gapOpen, gapExt) {

  /**
   * First string and first column for each matrix set 0.0
   */
  protected def initial(mt : MatrixType, dt : DimensionType) : Int => Int = mt match{
    case matrixTypes => (i: Int) => 0
  }

  override protected def matrixValue(i : Int, j : Int) : MatrixType => Int = {
    case MatrixType.Primary =>
      val tmp1 = math.max(matrixValue(i, j)(MatrixType.Horizontal), matrixValue(i, j)(MatrixType.Vertical))
      val tmp2 = math.max(matrixValue(i, j)(MatrixType.Substitution), 0)
      math.max(tmp1, tmp2)
    case MatrixType.Horizontal =>
      math.max(matrix(MatrixType.Horizontal)(i, j - 1) + gapExt, matrix(MatrixType.Primary)(i, j - 1) + gapOpen)
    case MatrixType.Vertical =>
      math.max(matrix(MatrixType.Vertical)(i - 1, j) + gapExt, matrix(MatrixType.Primary)(i - 1, j) + gapOpen)
    case MatrixType.Substitution =>
      matrix(MatrixType.Primary)(i - 1, j - 1) + subscore(i - 1, j - 1)
  }

  override protected def prepare(horizontal : Growable[Option[Symbol]], vertical : Growable[Option[Symbol]]) : (Int, (Int, Int)) = {
    var i = matrix(MatrixType.Primary).realHeight - 1
    var j = matrix(MatrixType.Primary).realWidth - 1
    var score = Int.MinValue

    val tmp: Array[(Int, Int)] = ((0 to i) cross (0 to j)).toArray
    cfor(0)(_ < tmp.length, _ + 1){ idx =>
      val (it, jt) = tmp(idx)
      if (score < matrix(MatrixType.Primary)(it, jt)) {
        score = matrix(MatrixType.Primary)(it, jt)
        i = it; j = jt
      }
    }
    (score, (i, j))
  }

  /**
   * condition for continue cycle process
   * continue until reached edge or `zero` score
   */
  override protected def cycleCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 && j != 0 && matrix(MatrixType.Primary)(i, j) != 0
}