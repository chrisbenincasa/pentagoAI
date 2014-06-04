//
// Matrix.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import scala.collection.mutable.ListBuffer

object Piece extends Enumeration {
  type Piece = Value
  val Black = Value(0, "b")
  val White = Value(1, "w")
  val Blank = Value(-1, ".")

  def safePieceFromString(str: String): Option[Piece] = {
    try {
      Some(Piece.withName(str))
    } catch {
      case e: NoSuchElementException => None
    }
  }
}

class Matrix[T](i: List[List[T]]) extends Iterable[Iterable[T]] {

  val rows = i.size
  val cols = i.headOption.map(_.size).getOrElse(0)

  override def iterator: Iterator[Iterable[T]] = new Iterator[Iterable[T]] {
    private var curr = i

    override def next(): Iterable[T] = {
      if (hasNext) {
        val ret = curr.head
        curr = curr.tail
        ret
      } else {
        Iterator.empty.next()
      }
    }

    override def hasNext: Boolean = !curr.isEmpty
  }

  def apply(row: Int, col: Int): T = {
    if (row > rows) throw new IndexOutOfBoundsException
    if (col > cols) throw new IndexOutOfBoundsException

    i(row)(col)
  }

  def row(idx: Int): List[T] = i(idx)

  def col(idx: Int): List[T] = i.map(_.apply(idx))

  def updated(row: Int, col: Int, v: T): Matrix[T] = {
    val newCol = i(row).updated(col, v)
    val newRow = i.updated(row, newCol)
    new Matrix(newRow)
  }

  def isDefinedAt(row: Int, col: Int): Boolean = {
    i.isDefinedAt(row) && i(row).isDefinedAt(col)
  }

  def transpose: Matrix[T] = {
    def transpose0(i: List[List[T]]): List[List[T]] = {
      if (i.head.isEmpty) {
        Nil
      } else i.map(_.head) :: transpose0(i.map(_.tail))
    }

    new Matrix(transpose0(i))
  }

  def swapRows: Matrix[T] = {
    val buf = ListBuffer[List[T]](i: _*)
    var cntI = 0
    var cntJ = rows - 1
    while (cntI < cntJ) {
      buf.update(cntI, i.apply(cntJ))
      buf.update(cntJ, i.apply(cntI))
      cntI += 1
      cntJ -= 1
    }

    new Matrix(buf.toList)
  }

  def rotateLeft: Matrix[T] = transpose.swapRows

  def rotateRight: Matrix[T] = swapRows.transpose
}