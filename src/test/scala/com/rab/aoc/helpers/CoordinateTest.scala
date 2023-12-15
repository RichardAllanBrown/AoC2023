package com.rab.aoc.helpers

import com.rab.aoc.UnitSpec

import scala.util.Random

class CoordinateTest extends UnitSpec {
  def randCoords: Seq[Coordinate] = {
    val coords = Seq(Coordinate(0, 0), Coordinate(0, 1), Coordinate(1, 0), Coordinate(1, 1))
    Random.shuffle(coords)
  }

  test("Can order coordinates as expected") {
    val result = randCoords.sorted(Coordinate.LeftToRightTopToBottomOrdering)
    result shouldEqual Seq(Coordinate(0, 0), Coordinate(1, 0), Coordinate(0, 1), Coordinate(1, 1))
  }
}
