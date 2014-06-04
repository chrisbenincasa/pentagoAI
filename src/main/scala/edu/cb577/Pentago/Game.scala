//
// Game.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.PlayerType.PlayerType
import java.io.{FileNotFoundException, FileInputStream, File}
import scala.annotation.tailrec

object Game extends App {
  override def main(args: Array[String]): Unit = {
    val configurationFile = new File("src/main/resources/test-002")
    val configuration = try {
      val is = new FileInputStream(configurationFile)
      Configuration.parseConfiguration(is)
    } catch {
      case e: FileNotFoundException => {
        e.printStackTrace()
        None
      }
    }

    val (player1, player2) = configuration.map(getPlayers).getOrElse {
      (new HumanPlayer("Human", Piece.White), new ComputerPlayer("Computer", Piece.Black) with Negamax with Heuristic1)
    }

    val board = configuration.map(_.board).getOrElse(Board.EMPTY_BOARD)

    // TODO check on Windows
    // print(27.toChar + "[2J") // clear screen

    new Game(board, player1, player2, configuration.map(_.whoMakesNextMove).getOrElse(1)).play
  }

  private def getPlayers(config: Configuration): (Player, Player) = {
    println("Loaded configuration file.")

    def getPlayerType(num: Int, name: String): PlayerType = {
      var playerTypeStr = ""
      do {
        playerTypeStr = readLine("Please enter the playerType for player %d (\"human\" or \"computer\") (%s)\n".format(num, name)).trim
      } while (!isValidPlayerType(playerTypeStr))

      PlayerType.safePlayerTypeFromString(playerTypeStr).get
    }

    val player1Type = getPlayerType(1, config.player1Name)
    val player2Type = getPlayerType(2, config.player2Name)

    (Player.playerForPlayerType(config.player1Name, config.player1Token, player1Type),
      Player.playerForPlayerType(config.player2Name, config.player2Token, player2Type))
  }

  private def isValidPlayerType(str: String): Boolean = PlayerType.safePlayerTypeFromString(str.toLowerCase).isDefined
}

class Game(var board: Board, player1: Player, player2: Player, var whoMovesNext: Int) {
  @tailrec
  final def play: Unit = {
    print(27.toChar + "[2J") // clear screen
    print(27.toChar + "[;H") // move cursor to top left
    println(board)
    val moveToApply = getNextMove
    board = board.applyMove(moveToApply)
    println("Applying " + moveToApply.toString)
    println("\r**********************")
    (GameHelper.didPieceWin(board, player1.piece), GameHelper.didPieceWin(board, player2.piece)) match {
      case (true, true)   => println(board); println("It was a tie!")
      case (false, true)  => println(board); println("Player 2 wins!")
      case (true, false)  => println(board); println("Player 1 wins!")
      case (false, false) => play
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
    move
  }
}
