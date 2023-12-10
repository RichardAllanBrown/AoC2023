package com.rab.aoc.helpers

import com.rab.aoc.UnitSpec

class GridTest extends UnitSpec {
  val grid = Grid(4, 4, "ABCD1234EFGH5678".toCharArray)

  test("Get will return expected value") {
    grid.get(Coordinate(0, 0)) shouldEqual 'A'
    grid.get(Coordinate(3, 0)) shouldEqual 'D'
    grid.get(Coordinate(2, 2)) shouldEqual 'G'
    grid.get(Coordinate(3, 3)) shouldEqual '8'
    grid.get(Coordinate(0, 1)) shouldEqual '1'
  }

  test("Find first returns expected value") {
    grid.findFirstPoint(_ == 'D') shouldEqual Some(Coordinate(3, 0))
    grid.findFirstPoint(_ == '1') shouldEqual Some(Coordinate(0, 1))
    grid.findFirstPoint(_ == '8') shouldEqual Some(Coordinate(3, 3))
  }

  test("Get cardinal neighbours returns expected values at corner") {
    val result = grid.getCardinalNeighbouringPoints(Coordinate(0, 0)).toSet
    result shouldEqual Set(Coordinate(0, 1), Coordinate(1, 0))
  }

  test("Get cardinal neighbours returns expected value in middle") {
    val result = grid.getCardinalNeighbouringPoints(Coordinate(2, 2)).toSet
    result shouldEqual Set(Coordinate(2, 1), Coordinate(1, 2), Coordinate(2, 3), Coordinate(3, 2))
  }
}
