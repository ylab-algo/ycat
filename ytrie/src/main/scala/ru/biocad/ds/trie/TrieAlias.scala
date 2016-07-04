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

package ru.biocad.ds.trie

import spire.implicits._

/**
 * User: pavel
 * Date: 29.07.15
 * Time: 20:38
 */

sealed trait CachedDepth[Symbol, Data] extends ITrie[Symbol, Data] {

  val trie : ITrie[Symbol, _]

  private[this] var _maxDepth = 0
  private[this] var _cachedSize = 0

  override def depth : Int = {
    if (_cachedSize != trie.size) {
      _cachedSize = trie.size
      dfs(trie.root, 0)
    }
    _maxDepth
  }

  private[this] def dfs(node: TrieData[Symbol, _], dzero: Int): Unit = {
    if (dzero > _maxDepth) _maxDepth = dzero
    node.children.foreach(n => dfs(n, dzero + 1))
  }
}


class TrieAlias[Symbol, @specialized(Boolean) Data](override val trie : ITrie[Symbol, _]) extends CachedDepth[Symbol, Data] {
  private[this] val _data = {
    val tmp: Array[TrieData[Symbol, Data]] = new Array(trie.size)
    cforRange(0 until trie.size){ i => tmp(i) = new TrieData[Symbol, Data](trie(i)) }
    tmp
  }

  override def root : TrieData[Symbol, Data] = _data(trie.root.id)
  override def apply(idx : Int) : TrieData[Symbol, Data] = _data(idx)
  override def toRoot(td : TrieData[Symbol, _]) : Iterator[TrieData[Symbol, Data]] =
    trie.toRoot(trie(td.id)).map(td => apply(td.id))

  override def size : Int = trie.size

  override def reverseIterator : Iterator[TrieData[Symbol, Data]] = trie.reverseIterator.map(td => apply(td.id))
  override def iterator : Iterator[TrieData[Symbol, Data]] = trie.iterator.map(td => apply(td.id))
}


class ConstrainedTrieAlias[Symbol, Data](override val trie : ITrie[Symbol, Data])
                                        (condition : TrieData[Symbol, Data] => Boolean) extends CachedDepth[Symbol, Data] {
  private[this] val _data = {
    val tmp: Array[Option[TrieData[Symbol, Data]]] = new Array(trie.size)
    cforRange(0 until trie.size){ i => tmp(i) = Option(trie(i)).filter(condition) }
    tmp
  }

  override def root : TrieData[Symbol, Data] = _data(trie.root.id).get
  override def apply(idx : Int) : TrieData[Symbol, Data] = _data(idx).get
  override def toRoot(td : TrieData[Symbol, _]) : Iterator[TrieData[Symbol, Data]] =
    trie.toRoot(trie(td.id)).map(td => apply(td.id))

  override def size : Int = trie.size

  override def reverseIterator : Iterator[TrieData[Symbol, Data]] = trie.reverseIterator.flatMap(td => _data(td.id))
  override def iterator : Iterator[TrieData[Symbol, Data]] = trie.iterator.flatMap(td => _data(td.id))
}