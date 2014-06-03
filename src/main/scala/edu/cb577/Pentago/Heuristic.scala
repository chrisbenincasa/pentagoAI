//
// Heuristic.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import scala.collection.mutable.ListBuffer
import edu.cb577.Pentago.Piece.Piece

trait Heuristic {
  def evaluate(board: Board, piece: Piece): Long
}

trait Heuristic1 extends Heuristic {
  def evaluate(board: Board, piece: Piece): Long = {
    val start = System.currentTimeMillis()
    var score = 0L

    val indexesToSkip = new ListBuffer[(Int, Int)]()
    val boardList = board.asLists
    val wholeBoard = new Matrix(boardList)

    def check(row: Int, col: Int, deltas: List[(Int, Int)]): Long = {
      if (indexesToSkip.contains((row, col))) return 0

      val toCheck = deltas.map {
        case (rowD, colD) => (row + rowD, col + colD)
      }

      val horizontalCount = toCheck.count {
        case (i, j) => wholeBoard.isDefinedAt(i, j) && wholeBoard.apply(i, j) == piece
      }

      if (horizontalCount >= 2) {
        indexesToSkip ++= toCheck
      }

      horizontalCount + 1
    }

    val indexes = for {
      (row, i) <- boardList.zipWithIndex
      (col, j) <- row.zipWithIndex
      if col == piece
    } yield (i, j)

    val filteredCounts = indexes.map {
      case (i, j) => GameHelper.winLists.map(list => check(i, j, list))
    }.flatten.filter(_ >= 3)

    val countMap = (3 to 5).map(num => (num, filteredCounts.count(_ == num))).toMap

    score += 100L * countMap.get(3).getOrElse(0)
    score += 10000L * countMap.get(4).getOrElse(0)
    score += 100000L * countMap.get(5).getOrElse(0)

    (1 to 4).foreach(num => {
      if (board.pieceAtPosition(num, 5) == piece) score += 5
    })

    val end = System.currentTimeMillis()
    //    println("h1 took %d millis".format(end - start))
    score
  }
}
