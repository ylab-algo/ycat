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

package ru.biocad.ds.trie.internal

import scala.collection.mutable.{ArrayBuffer => MArrayBuffer}


/**
 * User: pavel
 * Date: 10.07.14
 * Time: 18:00
 */
case class Node[Symbol](symbol : Option[Symbol] = None,
                        id : Int = 0,
                        parent : Option[Node[Symbol]] = None) extends Ordered[Node[Symbol]] {

  /**
   * 23.10.2015 by Nikolay Donets
   * [[Node.apply]] and [[Node.get]] is used in [[ru.biocad.ds.trie.Trie]] and
   * [[TrieImpl]] in functions like insert and find. As the main purpose of a trie is to
   * store data and provide an opportunity to traverse this data iteratively,
   * it is reasonable to sacrifice lookup speed of [[scala.collection.mutable.HashTable]] by
   * memory efficiency of [[scala.collection.mutable.ArrayBuffer]]
   */
  private val _children = MArrayBuffer.empty[Node[Symbol]]

  val keys = _children.flatMap(_.symbol)
  val values = _children

  def size = _children.size
  def isEmpty = _children.isEmpty

  def apply(key: Symbol): Node[Symbol] = get(key) match {
    case None => throw new NoSuchElementException("key not found: " + key)
    case Some(value) => value
  }

  def get(key : Symbol) : Option[Node[Symbol]] = _children.find(_.symbol.contains(key))

  def iterator : Iterator[Node[Symbol]] = _children.iterator

  def +=(kv: Node[Symbol]) : Node.this.type = {
    _children += kv
    this
  }

  def -=(key: Symbol) : Node.this.type = {
    val idx = _children.indexWhere(_.symbol == key)
    _children.remove(idx)
    this
  }

  override def compare(that : Node[Symbol]) : Int = id compare that.id

  override def toString: String = s"Node($symbol: ${
    parent match {
      case Some(p) => p.id
      case None    =>
    }
  } -> $id)"
}