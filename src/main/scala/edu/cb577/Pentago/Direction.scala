//
// Direction.scala
//
// Copyright (c) 2014 by Curalate, Inc.
//

package edu.cb577.Pentago

object Direction extends Enumeration {
  type Direction = Value
  val Left = Value(0, "l")
  val Right = Value(1, "r")

  def safeDirectionFromString(str: String): Option[Direction] = {
    try {
      Some(Direction.withName(str.toLowerCase))
    } catch {
      case e: NoSuchElementException => None
    }
  }
}
