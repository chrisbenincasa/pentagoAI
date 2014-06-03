//
// Move.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Direction.Direction
import edu.cb577.Pentago.Piece.Piece

object Move {
  val MOVE_PATTERN = """(\d)/(\d)\s(\d)([L|R|l|r])""".r
}

case class Move(block: Int, position: Int, rotationBlock: Int, direction: Direction, piece: Piece) {
  override def toString: String = "%d/%d %d%s".format(block, position, rotationBlock, direction)
}
