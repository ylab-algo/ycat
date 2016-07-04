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

import ru.biocad.algo.alignment.internal.{Alignment, MatrixType}
import ru.biocad.common.Functional._
import ru.biocad.ds.matrix.Matrix
import ru.biocad.ds.matrix.internal.DimensionType
import spire.implicits._

/**
 * User: pavel
 * Date: 19.12.14
 * Time: 10:36
 */
/**
 * Abstract class for any Alignment with affine gap
 * @param width - matrix width
 * @param height - matrix height
 * @param sub - substitution score
 * @param gapOpen - gap open
 * @param gapExt - gap extension (number of gaps more than 1)
 * @tparam Symbol - type of sequence symbol (`AminoAcid`, `Nucleotide`)
 */
abstract class AffineAlignment[@specialized(Char) Symbol](width : Int,
                                                          height : Int,
                                                          sub : (Symbol, Symbol) => Int,
                                                          gapOpen : Int,
                                                          gapExt : Int) extends Alignment[Symbol] {

  private [this] val _matrix = MatrixType.ALL.map(t => t -> new Matrix[Symbol](width, height)).toMap

  /**
   * For each matrix set values for first string (`horizontal`) and column (`vertical`)
   */
  cforRange(MatrixType.ALL.indices){ i =>
    val t = MatrixType.ALL(i)
    val m = _matrix.get(t).get

    cfor(0)(_ < DimensionType.ALL.length, _ + 1) { j =>
      val d: DimensionType = DimensionType.ALL(j)
      m.dimension(d).move(1)
      m.set(0, initial(t, d))(d)
    }
  }

  /**
   * @param mt - matrix type (`primary`, `substitution`, `horizontal`, `vertical`)
   * @param dt - dimension type (`horizontal`, `vertical`)
   * @return - function which by index return score
   */
  protected def initial(mt : MatrixType, dt : DimensionType) : Int => Int

  override protected def matrixTypes : Array[MatrixType] = MatrixType.ALL // MatrixType._values.get

  override protected def matrix : (MatrixType) => Matrix[Symbol] = _matrix(_)

  /**
   * @param i - horizontal position
   * @param j - vertical position
   * @return - function which by matrix type return score
   */
  override protected def matrixValue(i : Int, j : Int) : MatrixType => Int = {
    case MatrixType.Primary =>
      val tmp1 = math.max(matrixValue(i, j)(MatrixType.Horizontal), matrixValue(i, j)(MatrixType.Vertical))
      math.max(tmp1, matrixValue(i, j)(MatrixType.Substitution))
    case MatrixType.Horizontal =>
      math.max(matrix(MatrixType.Horizontal)(i, j - 1) + gapExt, matrix(MatrixType.Primary)(i, j - 1) + gapOpen)
    case MatrixType.Vertical =>
      math.max(matrix(MatrixType.Vertical)(i - 1, j) + gapExt, matrix(MatrixType.Primary)(i - 1, j) + gapOpen)
    case MatrixType.Substitution =>
      matrix(MatrixType.Primary)(i - 1, j - 1) + subscore(i - 1, j - 1)
  }


  /**
   * Conditions that еру `Primary matrix` has a value some matrix (`substitution`, `horizontal`, `vertical`)
   */
  override protected def substitutionCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 && j != 0 && _matrix(MatrixType.Primary)(i, j) == _matrix(MatrixType.Substitution)(i, j)

  override protected def verticalCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 && (j == 0 || _matrix(MatrixType.Primary)(i, j) == _matrix(MatrixType.Vertical)(i, j))

  override protected def horizontalCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    j != 0 && (i == 0 || _matrix(MatrixType.Primary)(i, j) == _matrix(MatrixType.Horizontal)(i, j))

  override protected def substitution : (Symbol, Symbol) => Int = sub
}
