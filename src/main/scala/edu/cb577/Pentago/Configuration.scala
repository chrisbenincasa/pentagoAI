//
// Configuration.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece
import java.io.InputStream
import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Configuration(
  var player1Name: String,
  var player2Name: String,
  var player1Token: Piece,
  var player2Token: Piece,
  var whoMakesNextMove: Int,
//  var board: Board,
  var moveHistory: Queue[Move]
)

object Configuration {
  def parseConfiguration(in: InputStream): Option[Configuration] = {
    import Move._

    val scanner = new Scanner(in)
    def nextOption: Option[String] = if (scanner.hasNext) Some(scanner.next) else None

    try {
      val player1Name = nextOption.getOrElse(throw new RuntimeException)
      val player2Name = nextOption.getOrElse(throw new RuntimeException)
      val player1Token = nextOption.map(_.toLowerCase).flatMap(Piece.safePieceFromString).getOrElse(throw new RuntimeException)
      val player2Token = nextOption.map(_.toLowerCase).flatMap(Piece.safePieceFromString).getOrElse(throw new RuntimeException)

      if (!scanner.hasNextInt) throw new RuntimeException("Next line is not an integer")

      val whoMovesNext = scanner.nextInt

      if (whoMovesNext < 1 || whoMovesNext > 2) {
        throw new RuntimeException("Game configuration only supports 2 players. Value: %d".format(whoMovesNext))
      }

      val board = (0 until 6).map(_ => nextOption.getOrElse(throw new RuntimeException)).toList

      @tailrec
      def allLines(hasNext: Boolean, accum: List[String]): List[String] = {
        if (hasNext) {
          val next = scanner.nextLine.trim
          if (next.isEmpty) {
            allLines(scanner.hasNextLine, accum)
          } else allLines(scanner.hasNextLine, next :: accum)
        } else accum
      }

      val moveLines = allLines(scanner.hasNextLine, Nil)

      @tailrec
      def getMoves(moves: List[String], accum: Queue[Move], moveAccum: Int = 0): Queue[Move] = {
        moves match {
          case Nil => accum
          case x :: xs => {
            val move = x match {
              case MOVE_PATTERN(block, pos, rotate, direction) => {
                val dir = Direction.safeDirectionFromString(direction.toLowerCase).getOrElse(throw new RuntimeException)
                val playerWhoMadeMove = if (moveAccum % 2 == 0) player1Token else player2Token
                Move(block.toInt, pos.toInt, rotate.toInt, dir, playerWhoMadeMove)
              }
              case _ => throw new RuntimeException("Bad move")
            }
            getMoves(xs, accum.enqueue(move), moveAccum + 1)
          }
        }
      }

      val moves = getMoves(moveLines, Queue.empty[Move])

      Some(Configuration(
        player1Name,
        player2Name,
        player1Token,
        player2Token,
        whoMovesNext,
        moves
      ))
    } catch {
      case e: Exception => {
        println("error while reading configuration file", e.getMessage)
        e.printStackTrace()
        None
      }
    } finally {
      scanner.close()
    }
  }
}
