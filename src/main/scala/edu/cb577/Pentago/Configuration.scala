//
// Configuration.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece
import java.io.{FileOutputStream, File, InputStream}
import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Configuration(
  var configurationName: String,
  var player1Name: String,
  var player2Name: String,
  var player1Token: Piece,
  var player2Token: Piece,
  var whoMakesNextMove: Int,
  var board: Board,
  var moveHistory: Queue[Move]
) {
  val DEFAULT_PATH = "src/main/resources/"

  def writeConfiguration(): Unit = {
    val file = new File(DEFAULT_PATH + configurationName)
    val fos = new FileOutputStream(file, false) // Don't append, overwrite

    fos.write((player1Name + "\n").getBytes)
    fos.write((player2Name + "\n").getBytes)
    fos.write((player1Token.toString.capitalize + "\n").getBytes)
    fos.write((player2Token.toString.capitalize + "\n").getBytes)
    fos.write((whoMakesNextMove + "\n").getBytes)
    val boardString = board.asLists.map(row => row.mkString + "\n").mkString
    fos.write(boardString.getBytes)
    moveHistory.foreach(move => {
      fos.write((move.toString + "\n").getBytes)
    })

    fos.flush()
    fos.close()
  }
}

object Configuration {
  def parseConfiguration(configurationName: String, in: InputStream): Option[Configuration] = {
    import Move._

    val scanner = new Scanner(in)
    def nextOption: Option[String] = if (scanner.hasNextLine) Some(scanner.nextLine()) else None

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
      val (upper, lower) = board.splitAt(3)

      def getPieceMatrices(l: List[String]): List[Matrix[Piece]] = {
        val lSplit = l.map(_.splitAt(3))
        val (leftList, rightList) = lSplit.zipWithIndex.map {
          case ((left, right), idx) => {
            val leftList = left.map(_.toString).flatMap(Piece.safePieceFromString)
            val rightList = right.map(_.toString).flatMap(Piece.safePieceFromString)
            (leftList.toList, rightList.toList)
          }
        }.unzip

        List(new Matrix(leftList), new Matrix(rightList))
      }

      val matrices = getPieceMatrices(upper) ++ getPieceMatrices(lower)

      val boardObj = new Board(matrices)

      @tailrec
      def allLines(hasNext: Boolean, accum: List[String]): List[String] = {
        if (hasNext) {
          val next = scanner.nextLine.trim
          if (next.isEmpty) {
            allLines(scanner.hasNextLine, accum)
          } else allLines(scanner.hasNextLine, next :: accum)
        } else accum
      }

      if (scanner.hasNextLine) scanner.nextLine()
      val moveLines = allLines(scanner.hasNextLine, Nil)
      println(moveLines)

      @tailrec
      def getMoves(moves: List[String], accum: Queue[Move], moveAccum: Int = 0): Queue[Move] = {
        moves match {
          case Nil => accum
          case x :: xs if x.length == 0 => getMoves(xs, accum, moveAccum)
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
        configurationName,
        player1Name,
        player2Name,
        player1Token,
        player2Token,
        whoMovesNext,
        boardObj,
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
