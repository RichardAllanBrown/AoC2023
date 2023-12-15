package com.rab.aoc.helpers

case class Coordinate(x: Int, y: Int) {
  lazy val cardinalNeighbours: Seq[Coordinate] = Seq(up, down, left, right)
  lazy val up: Coordinate = copy(y = y - 1)
  lazy val down: Coordinate = copy(y = y + 1)
  lazy val left: Coordinate = copy(x = x - 1)
  lazy val right: Coordinate = copy(x = x + 1)
}

object Coordinate {
  implicit object LeftToRightTopToBottomOrdering extends Ordering[Coordinate] {
    override def compare(a: Coordinate, b: Coordinate): Int = {
      val yComp = a.y.compare(b.y)
      if yComp == 0 then a.x.compare(b.x) else yComp
    }
  }
}

case class LongCoordinate(x: Long, y: Long) {
  def manhattanDistance(other: LongCoordinate): Long = {
    math.abs(x - other.x) + math.abs(y - other.y)
  }
}
