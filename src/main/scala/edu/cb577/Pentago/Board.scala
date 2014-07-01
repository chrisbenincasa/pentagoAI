//
// Board.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Board {
  private val ROW_SEPARATOR = "+-------+-------+\n"
  private val ROW_FORMAT = "| %s | %s |\n"

  val EMPTY_BOARD: Board = {
    val blank = List((0 until 3).map(_ => Piece.Blank): _*)
    val emptyMatrix = new Matrix((0 until 3).map(_ => blank).toList)//.asMatrix
    val board = (0 to 3).flatMap(_ => List(emptyMatrix)).toList
    new Board(board)
  }
}

case class Board(private val board: List[Matrix[Piece]]) {
  import Board._

  /**
   * Retrieve the Piece at the given block/position of the board
   * Expected 1-based indexes
   * @return the piece type at the given position
   */
  def pieceAtPosition(block: Int, pos: Int): Piece = board.apply(block - 1).apply((pos - 1) / 3, (pos- 1) % 3)

  /**
   * Retrieve an entire block from the Board
   * @return the specified block
   */
  def getBlock(block: Int): Matrix[Piece] = board.apply(block - 1)

  /**
   * Apply a move to the Board and return a new Board
   * @param move move to apply
   * @return the new Board
   */
  def applyMove(move: Move): Board = {
    val moveBlock = move.block - 1
    val rotationBlock = move.rotationBlock - 1
    val row = (move.position - 1) / 3
    val col = (move.position - 1) % 3

    val updatedMatrix = board(moveBlock).updated(row, col, move.piece)
    val updatedBoard1 = board.updated(moveBlock, updatedMatrix)
    val rot = move.direction match {
      case Direction.Left  => updatedBoard1(rotationBlock).rotateLeft
      case Direction.Right => updatedBoard1(rotationBlock).rotateRight
    }
    val updatedBoard2 = updatedBoard1.updated(move.rotationBlock - 1, rot)
    new Board(updatedBoard2)
  }

  /**
   * Apply a series of moves to a board and return the resulting board
   * @return the resulting board
   */
  def applyMoves(moves: Queue[Move]): Board = {
    @tailrec
    def applyMoves0(moves: Queue[Move], board: Board): Board = {
      if (moves.isEmpty) board else applyMoves0(moves.tail, board.applyMove(moves.head))
    }

    applyMoves0(moves, this)
  }

  /**
   * Retrieve the number of non-blank pieces currently on the board
   */
  def pieceCount: Int = {
    (for {
      block <- 1 to 4
      position <- 1 to 9
      if pieceAtPosition(block, position) != Piece.Blank
    } yield true).length
  }

  /**
   * Retrieve the board as a series of list where each list represents a single row of pieces
   * @return
   */
  def asLists: List[List[Piece]] = {
    def merge(a: Matrix[Piece], b: Matrix[Piece]): List[List[Piece]] = {
      (a zip b).map {
        case (a1, b1) => (a1 ++ b1).toList
      }.toList
    }

    merge(getBlock(1), getBlock(2)) ++ merge(getBlock(3), getBlock(4))
  }

  override def toString: String = {
    val lists = this.asLists
    val formatRow: (List[List[Piece]]) => (String => String) = l => {
      val str = l.map(row => {
        val (left, right) = row.map(_.toString).splitAt(3)
        ROW_FORMAT.format(left.mkString(" "), right.mkString(" "))
      }).mkString

      _ + str
    }

    val addRowSeparator: (String => String) = _ + ROW_SEPARATOR

    formatRow(lists.take(3)).
      andThen(addRowSeparator).
      andThen(formatRow(lists.drop(3).take(3))).
      andThen(addRowSeparator).
      apply(ROW_SEPARATOR)
  }
}
