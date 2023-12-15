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
}
