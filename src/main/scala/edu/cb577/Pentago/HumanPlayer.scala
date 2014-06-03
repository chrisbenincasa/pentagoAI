//
// HumanPlayer.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece

class HumanPlayer(name: String, piece: Piece) extends Player(name, piece) {
  override def nextMove(board: Board): Move = {
    import Move._

    val move = readLine("Please enter your move in the format \"{block}/{position} {rotateBlock}{rotateDirection}\"\n")
    move match {
      case MOVE_PATTERN(block, pos, rotate, dir) => {
        val direction = Direction.safeDirectionFromString(dir).getOrElse(throw new RuntimeException)
        Move(block.toInt, pos.toInt, rotate.toInt, direction, piece)
      }
      case _ => println("that move sucked"); throw new RuntimeException
    }
  }
}
