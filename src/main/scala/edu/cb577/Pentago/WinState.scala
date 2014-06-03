//
// WinState.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece

object WinState extends Enumeration {
  type WinState = Value
  val Win = Value(0, "win")
  val Draw = Value(1, "draw")
  val Lose = Value(2, "lose")
}

case class EndState(winner: Option[Piece], loser: Option[Piece])
