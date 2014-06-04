//
// AdversarialSearch.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece
import scala.util.control.Breaks._
import scala.util.Random

trait AdversarialSearch {
  // Must have a Heuristic
  this: Heuristic =>

  protected var startTime: Long = 0L
  protected val timeBound: Option[Long]
  protected val depthBound: Option[Int]
  protected val depthFunction: Option[(Board => Int)]

  val piece: Piece

  // Must me mixed in with something that defines an evaluation function
  def evaluate(board: Board, piece: Piece): Long

  // Extending class/trait must implement this function
  def search(board: Board): Move

  protected def allPossibleMoves(board: Board): Set[Move] = {
    val rows = (1 to 4).map(num => board.getBlock(num)).map(_.toIterable.flatten).map(_.zipWithIndex.map {
      case (t1, t2) => (t1, t2 + 1)
    })

    (for {
      (row, block) <- rows.zipWithIndex.map(t => (t._1, t._2 + 1))
      (p, pos) <- row
      if p == Piece.Blank
      rot <- 1 to 4
      direction <- Direction.values.toList
    } yield {
      Move(block, pos, rot, direction, piece)
    }).toSet
  }

  // Utility functions
  protected def startTimer: Unit = startTime = System.currentTimeMillis()

  protected def isPastTimeBound: Boolean = timeBound.map(t => (System.currentTimeMillis() - startTime) >= t).exists(identity)
}

trait Negamax extends AdversarialSearch {
  this: AdversarialSearch with Heuristic =>

  def search(board: Board): Move = {
    val moves = allPossibleMoves(board).
      filter(GameHelper.isValidMove(board, _)).
      toList.map(m => (m, board.applyMove(m))).
      sortBy(t => evaluate(t._2, piece)).reverse

    val depth = depthFunction.map(_.apply(board)).orElse(depthBound)

    val movesAndBoards = moves.par.map {
      case (m, newBoard) => (m, -negamax(newBoard, depth, Long.MinValue, Long.MaxValue, color = -1))
    }

    movesAndBoards.maxBy(_._2)._1
  }

  /**
   * Recursive implementation of the Negamax algorithm with alpha-beta pruning
   * In addition, this implementation is time-bounded by a constant initialized above
   * @return
   */
  private def negamax(node: Board, depth: Option[Int], a: Long, b: Long, color: Int): Long = {
    var alpha = a
    val beta = b

    val children = allPossibleMoves(node).map(move => (move, node.applyMove(move))).toList.sortBy(tup => evaluate(tup._2, piece))

    if (depth.map(_ == 0).exists(identity) || children.length == 0 || isPastTimeBound) {
      return color * evaluate(node, piece)
    }

    var bestValue = Long.MinValue

    (if (color == 1) children else children.reverse).foreach {
      case (move, board) => {
        val negaVal = -negamax(board, depth.map(_ - 1), -beta, -alpha, -color)
        bestValue = math.max(bestValue, negaVal)
        alpha = math.max(alpha, negaVal)
        if (alpha >= beta) return alpha
      }
    }

    bestValue
  }
}

trait Minimax extends AdversarialSearch {
  this: AdversarialSearch with Heuristic =>

  def search(board: Board): Move = {
    val moves = allPossibleMoves(board).
      filter(GameHelper.isValidMove(board, _)).
      toList.map(m => (m, board.applyMove(m))).
      sortBy(t => evaluate(t._2, piece)).reverse

    val movesAndBoards = moves.par.map {
      case (m, newBoard) => (m, minimax(newBoard, depthBound, Long.MaxValue, Long.MinValue, isMaximizing = false))
    }

    movesAndBoards.minBy(_._2)._1
  }

  private def minimax(node: Board, depth: Option[Int], alpha: Long, beta: Long, isMaximizing: Boolean): Long = {
    val possibleMoves = allPossibleMoves(node).toList.map(move => (move, node.applyMove(move))).sortBy(tup => evaluate(tup._2, piece))

    if (depth.map(_ == 0).exists(identity) || possibleMoves.length == 0 || isPastTimeBound) return evaluate(node, piece)

    var a = alpha
    var b = beta
    breakable {
      val moves = if (isMaximizing) possibleMoves.reverse else possibleMoves

      if (isMaximizing) {
        assert(evaluate(moves.head._2, piece) >= evaluate(moves.last._2, piece))
      } else {
        assert(evaluate(moves.head._2, piece) <= evaluate(moves.last._2, piece))
      }

      moves.foreach {
        case (move, newBoard) => {
          val score = minimax(newBoard, depth.map(_ - 1), a, b, !isMaximizing)
          if (isMaximizing) {
            a = math.max(a, score)
          } else {
            b = math.min(b, score)
          }
          if (b <= a) break()
        }
      }
    }

    if (isMaximizing) a else b
  }
}

trait PVS extends AdversarialSearch {
  this: AdversarialSearch with Heuristic =>

  def search(board: Board): Move = {
    val moves = allPossibleMoves(board).
      filter(GameHelper.isValidMove(board, _)).
      toList.map(m => (m, board.applyMove(m))).
      sortBy(t => evaluate(t._2, piece)).reverse

    val movesAndBoards = moves.par.map {
      case (m, newBoard) => (m, -pvs(newBoard, depthBound, Long.MinValue, Long.MaxValue, color = -1))
    }

    movesAndBoards.maxBy(_._2)._1
  }

  private def pvs(node: Board, depth: Option[Int], alpha: Long, beta: Long, color: Int): Long = {
    var a = alpha
    val b = beta
    var score = 0L
    val possibleMoves = allPossibleMoves(node).toList.map(move => (move, node.applyMove(move))).sortBy(tup => color * evaluate(tup._2, piece))

    if (depth.map(_ == 0).exists(identity) || possibleMoves.length == 0 || isPastTimeBound) {
      return color * evaluate(node, piece)
    }

    breakable {
      possibleMoves.map(_._2).zipWithIndex.foreach {
        case (newBoard, idx) => {
          if (idx != 0) {
            score = -pvs(newBoard, depth.map(_ - 1), -a - 1, -a, -color)
            if (a < score || score < b) {
              score = -pvs(newBoard, depth.map(_ - 1), -beta, -score, -color)
            }
          } else {
            score = -pvs(newBoard, depth.map(_ - 1), -beta, -alpha, -color)
          }
          a = math.max(a, score)
          if (a >= b) break()
        }
      }
    }

    a
  }
}

trait FlailWildly extends AdversarialSearch {
  this: AdversarialSearch with Heuristic =>

  def search(board: Board): Move = {
    val rand = new Random()
    val moves = allPossibleMoves(board)
    moves.toList.apply(rand.nextInt(moves.size - 1))
  }
}