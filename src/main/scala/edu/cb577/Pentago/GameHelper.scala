//
// GameHelper.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece._

object GameHelper {
  val horizonalDeltas = List((0, -2), (0, -1), (0, 1), (0, 2))
  val verticalDeltas = List((-2, 0), (-1, 0), (1, 0), (2, 0))
  val diagonalDeltas = List((-2, -2), (-1, -1), (1, 1), (2, 2)) :: List((-2, 2), (-1, 1), (1, -1), (2, -2)) :: Nil
  val winLists = List(horizonalDeltas, verticalDeltas) ::: diagonalDeltas

  /**
   * Checks if game is in a win state
   * @return Winner/Loser = Some(EndState(Some(winner), Some(loser)))
   *         Draw         = Some(EndState(None, None))
   *         No Winner    = None
   */
  def didPieceWin(board: Board, piece: Piece): Boolean = {
    val boardList = board.asLists
    val wholeBoard = new Matrix(boardList)

    def check(row: Int, col: Int): Boolean = {
      val indexesToCheck = winLists.map(list => {
        list.map {
          case (deltaX, deltaY) => (row + deltaX, col + deltaY)
        }
      })

      indexesToCheck.map(indexes => {
        indexes.forall {
          case (i, j) => wholeBoard.isDefinedAt(i, j) && wholeBoard.apply(i, j) == piece
        }
      }).reduceLeft(_ || _)
    }

    val indexes = for {
      (row, i) <- boardList.zipWithIndex
      (col, j) <- row.zipWithIndex
      if col == piece
    } yield (i, j)

    indexes.exists {
      case (row, col) => check(row, col)
    }
  }

  def isValidMove(board: Board, move: Move): Boolean = {
    board.pieceAtPosition(move.block, move.position) == Piece.Blank
  }
}
