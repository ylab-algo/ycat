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

package ru.biocad.ds.matrix

import ru.biocad.ds.matrix.internal.{DimensionType, MatrixDimension}
import spire.implicits._

/**
 * User: pavel
 * Date: 15.07.14
 * Time: 12:01
 */
class Matrix[@specialized(Char) Symbol](w : Int, h : Int) {
  private [this] val _matrix = new Array[Int](height * width)

  private [this] val horizontal = new MatrixDimension[Symbol](width)
  private [this] val vertical   = new MatrixDimension[Symbol](height)

  def apply(row : Int, col : Int) : Int =
    if (col < horizontal.realSize && row < vertical.realSize) {
      _matrix(width * row + col)
    }
    else throw new ArrayIndexOutOfBoundsException(s"Index is more, than real matrix size: ($row, $col) " +
      s"for ${vertical.realSize}x${horizontal.realSize} matrix")

  def update(row : Int, col : Int, v : Int) : Unit =
    if (col < horizontal.realSize && row < vertical.realSize) {
      _matrix(width * row + col) = v
    }
    else throw new ArrayIndexOutOfBoundsException(s"Index is more, than real matrix size: ($row, $col) " +
      s"for ${vertical.realSize}x${horizontal.realSize} matrix")

  def set(i : Int, f : Int => Int) : DimensionType => Unit = {
    case DimensionType.Horizontal =>
      cforRange(0 until width){ col => _matrix(width * i + col) = f(col) }
    case DimensionType.Vertical =>
      cforRange(0 until height){ row => _matrix(width * row + i) = f(row) }
  }

  def dimension : DimensionType => MatrixDimension[Symbol] = {
   case DimensionType.Horizontal => horizontal
   case DimensionType.Vertical => vertical
  }

  def width : Int = w
  def height : Int = h

  def realWidth : Int = horizontal.realSize
  def realHeight : Int = vertical.realSize
}
