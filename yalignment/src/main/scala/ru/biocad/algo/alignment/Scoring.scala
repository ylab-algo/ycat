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

package ru.biocad.algo.alignment

import java.io.InputStream

import scala.io.Source

/**
 * User: pavel
 * Date: 21.07.14
 * Time: 10:19
 */
object Scoring {

  //it should crash on empty data or wrong matrix type (nucleotide instead of aminoacid and vice versa)
  val loadMatrix: Iterator[String] => (Char, Char) => Int = lines => {

    val it = lines.buffered.dropWhile(_.startsWith("#")) //skip comments until header with letters
    val letters = it.next().split(' ').filter(_ != "").mkString("")

    val char2char2doubleMap: Map[Char, Map[Char, Int]] = it.map { case line =>
      line(0) -> line.substring(1).split(' ').filter(_ != "").zipWithIndex.map { case (score, idx) =>
        letters(idx) -> score.toInt
      }.toMap
    }.toMap

    (x: Char, y: Char) => char2char2doubleMap(x)(y)
  }

  def loadMatrixFromResource(resource : String) : InputStream = getClass.getResourceAsStream(resource)

  def loadMatrix(resource : String) : (Char, Char) => Int =
    loadMatrix(Source.fromInputStream(loadMatrixFromResource(resource)).getLines())

  lazy val NUC11 = loadMatrix("/NUC1.1.txt")
  lazy val NUC44 = loadMatrix("/NUC4.4.txt")
  lazy val BLOSUM62 = loadMatrix("/BLOSUM62.txt")
  lazy val PAM250 = loadMatrix("/PAM250.txt")
}