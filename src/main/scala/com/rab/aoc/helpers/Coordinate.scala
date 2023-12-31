package com.rab.aoc.helpers

case class Coordinate(x: Int, y: Int) {
  lazy val cardinalNeighbours: Seq[Coordinate] = Seq(up, down, left, right)
  lazy val up: Coordinate = copy(y = y - 1)
  lazy val down: Coordinate = copy(y = y + 1)
  lazy val left: Coordinate = copy(x = x - 1)
  lazy val right: Coordinate = copy(x = x + 1)
  def moveOne(direction: Direction): Coordinate = moveN(1, direction)

  def moveN(dist: Int, direction: Direction): Coordinate = direction match {
    case Up => copy(y = y - dist)
    case Down => copy(y = y + dist)
    case Left => copy(x = x - dist)
    case Right => copy(x = x + dist)
  }

  def manhattanDistance(other: Coordinate): Int = {
    math.abs(x - other.x) + math.abs(y - other.y)
  }
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

  def moveN(dist: Long, direction: Direction): LongCoordinate = direction match {
    case Up => copy(y = y - dist)
    case Down => copy(y = y + dist)
    case Left => copy(x = x - dist)
    case Right => copy(x = x + dist)
  }
}
