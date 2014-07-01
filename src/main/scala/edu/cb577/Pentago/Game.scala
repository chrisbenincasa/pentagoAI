//
// Game.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.PlayerType.PlayerType
import java.io.{FileNotFoundException, FileInputStream, File}
import scala.annotation.tailrec
import edu.cb577.Pentago.Piece.Piece
import scala.collection.immutable.Queue
import scala.util.Random

object Game extends App {
  override def main(args: Array[String]): Unit = {
    val (first, second) = args.length match {
      case 1 => (args.headOption.map(_.trim), None)
      case 2 => (args.headOption.map(_.trim), Some(args.apply(1).trim))
      case _ => (None, None)
    }

    val isBoolean = first.flatMap(getBooleanSafe).isDefined

    val fileName = if (isBoolean) None else first

    val configuration = fileName.flatMap(file => {
      val configurationFile = new File(file)
      try {
        val is = new FileInputStream(configurationFile)
        val c = Configuration.parseConfiguration(file, is)
        println("Loaded configuration file.")
        c
      } catch {
        case e: FileNotFoundException => {
          None
        }
      }
    }).getOrElse({
      val (player1Name, player1Piece) = getPlayerInfo(1, None)
      val (player2Name, player2Piece) = getPlayerInfo(2, Some(player1Piece))

      val whoMovesNext = 1
      val board = Board.EMPTY_BOARD
      val moves = Queue.empty[Move]

      val configName = new Random().alphanumeric.take(8).mkString

      Configuration(configName, player1Name, player2Name, player1Piece, player2Piece, whoMovesNext, board, moves)
    })

    val shouldKillAfterMove = if (isBoolean) first.flatMap(getBooleanSafe).getOrElse(false) else second.flatMap(getBooleanSafe).getOrElse(false)

    val (player1, player2) = getPlayers(configuration.player1Name, configuration.player1Token, configuration.player2Name, configuration.player2Token)

    new Game(configuration, player1, player2, shouldKillAfterMove).play
  }

  private def getPlayers(player1Name: String, player1Token: Piece, player2Name: String, player2Token: Piece): (Player, Player) = {
    val player1Type = getPlayerType(1, player1Name)
    val player2Type = getPlayerType(2, player2Name)

    (Player.playerForPlayerType(player1Name, player1Token, player1Type),
      Player.playerForPlayerType(player2Name, player2Token, player2Type))
  }

  private def getPlayerInfo(num: Int, otherPiece: Option[Piece]): (String, Piece) = {
    var name: String = ""
    do {
      name = readLine("Please enter the name of player %d\n".format(num))
    } while (name.trim.length == 0)

    var pieceStr: String = ""
    do {
      pieceStr = readLine("Please enter the piece color for player %d. (W or B)\n".format(num))
    } while (Piece.safePieceFromString(pieceStr).isEmpty || otherPiece.map(_ == Piece.safePieceFromString(pieceStr).get).exists(identity))

    (name, Piece.safePieceFromString(pieceStr).get)
  }

  private def isValidPlayerType(str: String): Boolean = PlayerType.safePlayerTypeFromString(str.toLowerCase).isDefined

  private def getPlayerType(num: Int, name: String): PlayerType = {
    var playerTypeStr = ""
    do {
      playerTypeStr = readLine("Please enter the playerType for player %d (\"human\" or \"computer\") (%s)\n".format(num, name)).trim
    } while (!isValidPlayerType(playerTypeStr))

    PlayerType.safePlayerTypeFromString(playerTypeStr).get
  }

  private def getBooleanSafe(str: String): Option[Boolean] = {
    try {
      Some(str.trim.toBoolean)
    } catch {
      case e => None
    }
  }
}

class Game(config: Configuration, player1: Player, player2: Player, shouldKillAfterMove: Boolean) {
  var board: Board = config.board
  var whoMovesNext: Int = config.whoMakesNextMove

  @tailrec
  final def play: Unit = {
    print(27.toChar + "[2J") // clear screen
    print(27.toChar + "[;H") // move cursor to top left
    println(board)

    val moveToApply = getNextMove
    println("Applying " + moveToApply.toString)
    board = board.applyMove(moveToApply)
    config.board = board
    config.moveHistory = config.moveHistory.enqueue(moveToApply)
    config.writeConfiguration()

    println("\r**********************")
    (GameHelper.didPieceWin(board, player1.piece), GameHelper.didPieceWin(board, player2.piece)) match {
      case (true, true)   => println(board); println("It was a tie!")
      case (false, true)  => println(board); println("Player 2 wins!")
      case (true, false)  => println(board); println("Player 1 wins!")
      case (false, false) => if (shouldKillAfterMove) return else play
    }
  }

  private def getNextMove: Move = {
    val player = if (whoMovesNext == 1) player1 else player2
    println("%s is choosing a move...".format(player.name))
    var move = player.nextMove(board)
    while (!GameHelper.isValidMove(board, move)) {
      move = player.nextMove(board)
    }
    whoMovesNext = if (whoMovesNext == 1) 2 else 1
    config.whoMakesNextMove = whoMovesNext
    move
  }
}
