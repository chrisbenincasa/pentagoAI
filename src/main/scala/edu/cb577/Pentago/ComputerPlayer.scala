//
// ComputerPlayer.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

import edu.cb577.Pentago.Piece.Piece

class ComputerPlayer(name: String, piece: Piece) extends Player(name, piece) {
  // Must have a search method and heuristic
  this: AdversarialSearch with Heuristic =>

  override protected val timeBound: Option[Long] = Some(1000 * 15) // 15 seconds
  override protected val depthBound: Option[Int] = Some(3)

  // Extending class must either define this, or ComputerPlayer must be injected with
  // a trait that defines it. We use the latter throughout the program

  def nextMove(board: Board): Move = {
    val start = System.currentTimeMillis()
    val move = search(board)
    val end = System.currentTimeMillis()
//    println("Took %s %d millis to find a move".format(name, end - start))
    move
  }
}
