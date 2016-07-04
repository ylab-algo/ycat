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

import ru.biocad.common.Functional._
import ru.biocad.ds.trie.internal.{Node, TrieImpl}

import scala.collection.mutable.{ArrayBuffer => MArrayBuffer}


/**
 * User: pavel
 * Date: 14.07.14
 * Time: 10:21
 */
class Trie[@specialized(Char) Symbol, Data](terminate : Symbol) extends ITrie[Symbol, Data] {
  private[this] val _trie = new TrieImpl[Symbol]
  private[this] val _data = MArrayBuffer[TrieData[Symbol, Data]](new TrieData[Symbol, Data](_trie.root))
  @volatile private[this] var _cache: MArrayBuffer[TrieData[Symbol, Data]] = null
  @volatile private[this] var _inverseCache: MArrayBuffer[TrieData[Symbol, Data]] = null
  private[this] val monitor = new AnyRef
  private [this] var _depth = 0
  def depth = _depth

  def push(s : Seq[Symbol]) : TrieData[Symbol, Data] = {
    _depth = math.max(_depth, s.size)

    _data((s :+ terminate).foldLeft(_trie.root)((node, c) => {
      val n = _trie.insert(node, c)
      if (n.id == _data.size) {
        _data.append(new TrieData[Symbol, Data](n))
      }
      n
    }).id)
  }

  def find(s : Seq[Symbol]) : Option[TrieData[Symbol, Data]] =
    s.foldLeft(Option(_trie.root))((snode, c) =>
      snode.flatMap(_.get(c))).map(node => _data(node.id))

  override def root : TrieData[Symbol, Data] =
    _data(_trie.root.id)

  override def apply(idx : Int) : TrieData[Symbol, Data] = _data(idx)

  override def toRoot(td : TrieData[Symbol, _]) : Iterator[TrieData[Symbol, Data]] =
    new Iterator[TrieData[Symbol, Data]] {
      private[this] var nextValue = td
      private[this] var _hasNext = true

      override def hasNext : Boolean = _hasNext

      override def next() : TrieData[Symbol, Data] = {
        val currentValue = nextValue
        nextValue.parentId match {
          case Some(idx) =>
            nextValue = _data(idx)
          case None =>
            _hasNext = false
        }
        currentValue.asInstanceOf[TrieData[Symbol, Data]]
      }
    }
    //td #:: td.parentId.map(i => toRoot(_data(i))).getOrElse(Stream.empty)

  override def size : Int = _trie.size

  private def cache(inverse : Boolean) : Unit = {
    var tmp = if (inverse) _inverseCache else _cache

    if (tmp == null || tmp.size != _trie.size) {
      monitor.synchronized {
        tmp = if (inverse) _inverseCache else _cache
        if (tmp == null || tmp.size != _trie.size) {
          if (tmp == null) tmp = MArrayBuffer.empty[TrieData[Symbol, Data]]
          else tmp.clear()
          fix[Node[Symbol], Unit](f => (node: Node[Symbol]) => {
            tmp += _data(node.id)
            val values = node.values.sorted
            (if (inverse) values.reverseIterator else values).foreach(c => f(c))
          })(_trie.root)
          if (inverse) _inverseCache = tmp
          else _cache = tmp
        }
      }
    }
  }

  override def reverseIterator : Iterator[TrieData[Symbol, Data]] = {
    cache(inverse = true)
    _inverseCache.toIterator
  }

  override def iterator : Iterator[TrieData[Symbol, Data]] = {
    cache(inverse = false)
    _cache.toIterator
  }
}
