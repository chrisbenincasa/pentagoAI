//
// Heuristic.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import scala.collection.mutable.ListBuffer
import edu.cb577.Pentago.Piece.Piece
import scala.annotation.tailrec

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

trait Heuristic2 extends Heuristic {
  private val rightDiagonals =
    List((3, 0), (4, 1), (5, 2)) ::
    List((2, 0), (3, 1), (4, 2), (5, 3)) ::
    List((1, 0), (2, 1), (3, 2), (4, 3), (5, 4)) ::
    List((0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5)) :: Nil
  private val leftDiagonals =
    List((3, 5), (4, 4), (5, 3)) ::
    List((2, 5), (3, 4), (4, 3), (5, 2)) ::
    List((1, 5), (2, 4), (3, 3), (4, 2), (5, 1)) ::
    List((0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)) :: Nil

  def evaluate(board: Board, piece: Piece): Long = {
    val otherPiece = if (piece == Piece.White) Piece.Black else Piece.White
    evaluateInner(board, piece) - evaluateInner(board, otherPiece)
  }

  private def evaluateInner(board: Board, piece: Piece): Long = {
    val boardList = board.asLists
    val wholeBoard = new Matrix(boardList)
    val columns = (0 until wholeBoard.cols).map(wholeBoard.col).toList

    // For each row, how many of our pieces
    // subtract for opposite pieces in this row
    @tailrec
    def consecutiveCounts(l: List[Piece], pieceToCheck: Piece, accum: List[Int] = Nil): List[(Int, Int)] = {
      l match {
        case Nil => accum.groupBy(identity).mapValues(_.length).toList
        case x :: xs if x != pieceToCheck => consecutiveCounts(xs, pieceToCheck, accum)
        case x :: xs if x == pieceToCheck => {
          val pieces = l.takeWhile(_ == piece)
          consecutiveCounts(l.drop(pieces.length), pieceToCheck, pieces.length :: accum)
        }
      }
    }

    val rowPieceCount = boardList.flatMap(consecutiveCounts(_, piece))
    val columnPieceCount = columns.flatMap(consecutiveCounts(_, piece))

    // Check diagonals
    val rightDiagonalCount = rightDiagonals.map(coordinates => coordinates.map {
      case (row, col) => wholeBoard.apply(row, col)
    }).flatMap(consecutiveCounts(_, piece))

    val leftDiagonalCount = leftDiagonals.map(coordinates => coordinates.map {
      case (row, col) => wholeBoard.apply(row, col)
    }).flatMap(consecutiveCounts(_, piece))

    val allFrequencies = rowPieceCount ++ columnPieceCount ++ rightDiagonalCount ++ leftDiagonalCount
    val freqMap = allFrequencies.groupBy(_._1).mapValues(l => l.map(_._2).sum)

    val score3 = freqMap.get(3).map(_ * 100L).getOrElse(0L)
    val score4 = freqMap.get(4).map(_ * 10000L).getOrElse(0L)
    val score5 = freqMap.get(5).map(_ * 100000L).getOrElse(0L)

    val middles = (1 to 4).map(num => {
      if (board.pieceAtPosition(num, 5) == piece) 5 else 0
    }).sum

    score3 + score4 + score5 + middles
  }
}