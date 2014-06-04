//
// HumanPlayer.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Move.MOVE_PATTERN
import edu.cb577.Pentago.Piece.Piece

class HumanPlayer(name: String, piece: Piece) extends Player(name, piece) {
  override def nextMove(board: Board): Move = {

    println(27.toChar + "[11;H")
    var move = ""
    do {
      move = readLine("Please enter your move in the format \"{block}/{position} {rotateBlock}{rotateDirection}\"\n").trim
    } while (getMoveFromString(move).isEmpty)

    getMoveFromString(move).get
  }

  private def getMoveFromString(moveStr: String): Option[Move] = {
    moveStr match {
      case MOVE_PATTERN(block, pos, rotate, dir) => {
        val direction = Direction.safeDirectionFromString(dir).getOrElse(throw new RuntimeException)
        Some(Move(block.toInt, pos.toInt, rotate.toInt, direction, piece))
      }
      case _ => None
    }
  }
}
