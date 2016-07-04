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

package ru.biocad.algo.alignment.simple

import ru.biocad.algo.alignment.internal.{Alignment, MatrixType}
import ru.biocad.ds.matrix.Matrix
import ru.biocad.ds.matrix.internal.DimensionType
import spire.implicits._

/**
 * User: pavel
 * Date: 16.07.14
 * Time: 10:03
 */
abstract class SimpleAlignment[@specialized(Char) Symbol](width : Int,
                                                          height : Int,
                                                          sub : (Symbol, Symbol) => Int,
                                                          gap : Int) extends Alignment[Symbol] {

  private [this] val _matrix = new Matrix[Symbol](width, height)

  cforRange(DimensionType.ALL.indices) { i =>
    val d: DimensionType = DimensionType.ALL(i)
    _matrix.dimension(d).move(1)
    _matrix.set(0, initial)(d)
  }

  protected def initial : Int => Int

  protected def primaryMatrixValue(i : Int, j : Int) : Int = {
    val p1 = primary(i - 1, j) + gap
    val p2 = primary(i - 1, j - 1) + subscore(i - 1, j - 1)
    val p3 = primary(i, j - 1) + gap

    math.max(math.max(p1, p2), p3)
  }

  protected val primary : Matrix[Symbol] = _matrix

  override protected def matrixTypes : Array[MatrixType] = Array(MatrixType.Primary)

  override protected def matrixValue(i : Int, j : Int) : MatrixType => Int = {
    case MatrixType.Primary => primaryMatrixValue(i, j)
    case _                  => throw new IllegalArgumentException("Only primary matrix available for simple alignments")
  }

  override protected def matrix : (MatrixType) => Matrix[Symbol] = {
    case MatrixType.Primary => _matrix
    case _                  => throw new IllegalArgumentException("Only primary matrix available for simple alignments")
  }

  override protected def cycleCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 || j != 0

  override protected def substitutionCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 && j != 0 && primary(i, j) == primary(i - 1, j - 1) + subscore(i - 1, j - 1)

  override protected def verticalCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    i != 0 && (j == 0 || primary(i, j) == primary(i - 1, j) + gap)

  override protected def horizontalCondition(m : (MatrixType) => Matrix[Symbol])(i : Int, j : Int) : Boolean =
    j != 0 && (i == 0 || primary(i, j) == primary(i, j - 1) + gap)

  override protected def substitution : (Symbol, Symbol) => Int = sub
}
