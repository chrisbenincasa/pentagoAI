//
// Player.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece
import edu.cb577.Pentago.PlayerType.PlayerType

object PlayerType extends Enumeration {
  type PlayerType = Value
  val Human = Value(1, "human")
  val Computer = Value(2, "computer")

  def safePlayerTypeFromString(str: String): Option[PlayerType] = {
    try {
      Some(PlayerType.withName(str.toLowerCase))
    } catch {
      case e: NoSuchElementException => None
    }
  }
}

object Player {
  def playerForPlayerType(name: String, piece: Piece, playerType: PlayerType): Player = playerType match {
    case PlayerType.Human => new HumanPlayer(name, piece)
    case PlayerType.Computer => new ComputerPlayer(name, piece) with Negamax with Heuristic1
  }
}

abstract class Player(val name: String, val piece: Piece) {
  def nextMove(board: Board): Move
}
