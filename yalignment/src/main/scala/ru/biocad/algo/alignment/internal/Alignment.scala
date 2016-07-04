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

package ru.biocad.algo.alignment.internal

import ru.biocad.algo.alignment.AlignmentResult
import ru.biocad.ds.matrix.Matrix
import ru.biocad.ds.matrix.internal.DimensionType
import spire.implicits._

import scala.collection.mutable


/**
 * User: pavel
 * Date: 15.07.14
 * Time: 12:33
 */
trait Alignment[@specialized(Char) Symbol] extends Traceback[Symbol] {

  final private [this] val matrixTypeNumber = matrixTypes.length

  def pop(dt : DimensionType) : Unit = {
    cfor(0)(_ < matrixTypeNumber, _ + 1) { t =>
      matrix(matrixTypes(t)).dimension(dt).pop()
    }
  }

  def push(s : IndexedSeq[Symbol])(dt : DimensionType) : Unit = {
    val oldWidth  = realSize(DimensionType.Horizontal)
    val oldHeight = realSize(DimensionType.Vertical)

    cfor(0)(_ < matrixTypeNumber, _ + 1) { t=>
      matrix(matrixTypes(t)).dimension(dt).push(s)
    }

    if (realSize(DimensionType.Horizontal) != 1 && realSize(DimensionType.Vertical) != 1) {
      val (x1, x2, y1, y2) =
        dt match {
          case DimensionType.Horizontal =>
            (oldWidth, oldWidth + s.size, 1, oldHeight)
          case DimensionType.Vertical =>
            (1, oldWidth, oldHeight, oldHeight + s.size)
        }

      cforRange(y1 until y2){ i =>
        cforRange(x1 until x2){ j =>
          cfor(0)(_ < matrixTypeNumber, _ + 1){ t =>
            val tmp = matrixTypes(t)
            matrix(tmp)(i, j) = matrixValue(i, j)(tmp)
          }
        }
      }
    }
  }

  def traceback() : AlignmentResult[Symbol] = {
    val horizontal = mutable.ArrayBuffer.empty[Option[Symbol]]
    val vertical = mutable.ArrayBuffer.empty[Option[Symbol]]

    var (score, (i, j)) = prepare(horizontal = horizontal, vertical = vertical)

    val seqv = sequences(DimensionType.Vertical)
    val seqh = sequences(DimensionType.Horizontal)

    while (cycleCondition(matrix)(i, j)) {
      val c = if (i > 0) seqv(i - 1) else null.asInstanceOf[Symbol]
      val d = if (j > 0) seqh(j - 1) else null.asInstanceOf[Symbol]

      if (substitutionCondition(matrix)(i, j)) {
        vertical += Some(c)
        horizontal += Some(d)
        i -= 1; j -= 1
      }
      else if (verticalCondition(matrix)(i, j)) {
        vertical += Some(c)
        horizontal += None
        i -= 1
      }
      else if (horizontalCondition(matrix)(i, j)) {
        vertical += None
        horizontal += Some(d)
        j -= 1
      }
      else assert(assertion = false)
    }

    AlignmentResult(score = score,
                    horizontal = horizontal.reverse,
                    vertical = vertical.reverse,
                    horizontalStart = j,
                    verticalStart = i)
  }

  protected def matrixTypes : Array[MatrixType]

  protected def matrix : MatrixType => Matrix[Symbol]

  protected def substitution : (Symbol, Symbol) => Int

  protected def matrixValue(i : Int, j : Int) : MatrixType => Int

  protected def realSize : DimensionType => Int =
    matrix(MatrixType.Primary).dimension(_).realSize

  protected def sequences : DimensionType => Seq[Symbol] =
    matrix(MatrixType.Primary).dimension(_).sequence

  protected def subscore(i : Int, j : Int) : Int =
    substitution(sequences(DimensionType.Vertical)(i), sequences(DimensionType.Horizontal)(j))
}
