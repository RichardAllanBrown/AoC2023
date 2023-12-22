package com.rab.aoc

import com.rab.aoc.Day21.*
import com.rab.aoc.helpers.Coordinate

class Day21Test extends UnitSpec {
  val lines = List(
    "...........",
    ".....###.#.",
    ".###.##..#.",
    "..#.#...#..",
    "....#.#....",
    ".##..S####.",
    ".##..#...#.",
    ".......##..",
    ".##.#.####.",
    ".##..##.##.",
    "..........."
  )

  test("Can parse input") {
    val grid = parseInput(lines)
    grid.findFirstPoint(_ == Garden.Elf) shouldEqual Some(Coordinate(5, 5))
    grid.findPoints(_ == Garden.FlowerBed).length shouldEqual 40
  }

  test("With 2 steps") {
    val grid = parseInput(lines)
    val result = getAllDestinationsAfterSteps(grid, 2)
    result shouldEqual Set(Coordinate(5, 5), Coordinate(3, 5), Coordinate(5, 3), Coordinate(4, 6))
  }

  test("With 6 steps") {
    val grid = parseInput(lines)
    val destinations = getAllDestinationsAfterSteps(grid, 6)
    destinations.size shouldEqual 16
  }
}
