package com.rab.aoc.helpers

import com.rab.aoc.UnitSpec

class GridTest extends UnitSpec {
  private val grid = Grid(4, 4, "ABCD1234EFGH5678".toCharArray)

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

  test("Round trip with some coordinates") {
    val someBoolGrid = List(
      "##.#......#.##.",
      "###........####",
      "###..#..#..####",
      ".###..##..###.#",
      ".#.###..###...#",
      ".....####.....#",
      "#.###.##.###.##",
      "#............#.",
      "..###.##.###..#",
      "##.#.#..#.#.###",
      "...#..##..#....",
      "##.#.####.#.##.",
      "###.#.##.#.###.",
      "....######.....",
      "####......#####",
      "####......#####",
      "....######....."
    )
    val origGrid = Grid.fromInput(someBoolGrid).map(_ == '#')
    val truthyCoords =  origGrid.findPoints(identity).toSet
    val newGrid = Grid.fromCoordinates(truthyCoords)
    newGrid shouldEqual origGrid
  }

  test("Can rotate 1x1 grid") {
    val smallGrid = Grid(1, 1, Seq(1))
    smallGrid.rotateClockwise shouldEqual smallGrid
  }

  test("Can rotate 2x2 grid") {
    val twoByGrid = Grid(2, 2, Seq(1, 2, 3, 4))
    twoByGrid.rotateClockwise shouldEqual Grid(2, 2, Seq(3, 1, 4, 2))
  }

  test("Can rotate 2x3 grid") {
    val twoByThree = Grid(2, 3, Seq(1, 2, 3, 4, 5, 6))
    twoByThree.rotateClockwise shouldEqual Grid(3, 2, Seq(5, 3, 1, 6, 4, 2))
  }

  test("Rotating 4 times is the same grid") {
    grid.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise shouldEqual grid
  }
}
