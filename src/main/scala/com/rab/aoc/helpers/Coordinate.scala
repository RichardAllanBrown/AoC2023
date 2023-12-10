package com.rab.aoc.helpers

case class Coordinate(x: Int, y: Int) {
  lazy val cardinalNeighbours: Seq[Coordinate] = Seq(up, down, left, right)
  lazy val up: Coordinate = copy(y = y - 1)
  lazy val down: Coordinate = copy(y = y + 1)
  lazy val left: Coordinate = copy(x = x - 1)
  lazy val right: Coordinate = copy(x = x + 1)
}
