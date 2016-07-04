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

package ru.biocad.ds.matrix.internal

import spire.syntax.cfor._

import scala.collection.mutable

/**
 * User: pavel
 * Date: 15.07.14
 * Time: 12:10
 */
class MatrixDimension[@specialized(Char) Symbol](size : Int) {
  private [this] var _realSize = 0

  private [this] val _sizes = Array.ofDim[Int](size)
  private [this] var sizesPointer = 0

  private [this] val _stack = { new Array[Any](size) } .asInstanceOf[Array[Symbol]] // Avoid using of ClassTag
  private [this] var stackPointer = 0

  private [this] val _cachedSequence = mutable.ArrayBuffer.empty[Symbol]
  private [this] var _validCache = false

  def push(s : IndexedSeq[Symbol]) : Unit = {
    if (_realSize + s.size <= size) {
      val n = s.size

      _sizes(sizesPointer) = n
      sizesPointer += 1

      cfor(0)(_ < s.length, _ + 1) { i =>
        _stack(stackPointer + i) = s(i)
      }

      stackPointer += n
      _realSize += n

      _validCache = false
    }
    else {
      throw new ArrayIndexOutOfBoundsException("Cannot push")
    }
  }

  def pop() : Option[IndexedSeq[Symbol]] = {
    try {
      sizesPointer -= 1
      val n = _sizes(sizesPointer)

      _validCache = false
      val tmp = stackPointer

      _realSize -= n
      stackPointer -= n

      Some(_stack.slice(tmp - n, tmp).toIndexedSeq)
    } catch {
      case e : NoSuchElementException => None
    }
  }

  def move(i : Int) : Unit = _realSize += i

  def realSize : Int = _realSize

  def sequence : Seq[Symbol] =
    if (_validCache) _cachedSequence else {
      _validCache = true
      _cachedSequence.clear()

      cfor(0)(_ < stackPointer, _ + 1) { i =>
        _cachedSequence += _stack(i)
      }

      _cachedSequence
    }
}
