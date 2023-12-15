package com.rab.aoc

import com.rab.aoc.Day14._

class Day14Test extends UnitSpec {
  val start = List(
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#...."
  )

  val expected = List(
    "OOOO.#.O..",
    "OO..#....#",
    "OO..O##..O",
    "O..#.OO...",
    "........#.",
    "..#....#.#",
    "..O..#.O.O",
    "..O.......",
    "#....###..",
    "#....#...."
  )

  test("Tilting north produces expected result") {
    tiltUp(parse(start)) shouldEqual parse(expected)
  }

  test("Computes load correctly") {
    computeTopLoad(parse(expected)) shouldEqual 136
  }

  val expectedPostCycle1 = List(
    ".....#....",
    "....#...O#",
    "...OO##...",
    ".OO#......",
    ".....OOO#.",
    ".O#...O#.#",
    "....O#....",
    "......OOOO",
    "#...O###..",
    "#..OO#...."
  )

  test("Will complete a cycle as expected") {
    cycle(parse(start)) shouldEqual parse(expectedPostCycle1)
  }

  val expectedPostCycle3 = List(
    ".....#....",
    "....#...O#",
    ".....##...",
    "..O#......",
    ".....OOO#.",
    ".O#...O#.#",
    "....O#...O",
    ".......OOO",
    "#...O###.O",
    "#.OOO#...O"
  )

  test("Will compute right state after 3 cycles") {
    cycleManyTimes(3, Map.empty)(parse(start)) shouldEqual parse(expectedPostCycle3)
  }

  test("After a billion cycles, computes right load") {
    solvePart2(start) shouldEqual 64
  }
}
