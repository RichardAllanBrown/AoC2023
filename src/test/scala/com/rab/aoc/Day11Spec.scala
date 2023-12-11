package com.rab.aoc

import com.rab.aoc.Day11._

class Day11Spec extends UnitSpec {
  private val lines = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  test("Part 1 works for example") {
    solvePart1(lines) shouldEqual 374
  }

  test("The part 2 for small expansion works") {
    val coordinates = parseInput(lines)
    val expandedCoords = expandUniverse(coordinates, 10)
    computeDistances(expandedCoords) shouldEqual 1030
  }
}
