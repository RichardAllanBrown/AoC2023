package com.rab.aoc

import com.rab.aoc.Day13._

class Day13Test extends UnitSpec {
  val exampleLines = List(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  )

  test("Can parse example into 2 grids") {
    parseMany(exampleLines).length shouldEqual 2
  }

  test("Can compute vertical reflection") {
    findVerticalReflection(parse(exampleLines.take(7))) shouldEqual Seq(5)
  }

  test("Can compute horizontal reflection") {
    findHorizontalReflection(parse(exampleLines.drop(8))) shouldEqual Seq(4)
  }

  test("Computes score") {
    solvePart1(exampleLines) shouldEqual 405
  }
  
  test("Solves complex instance") {
    val number97 = parse(List(
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
    ))
    findHorizontalReflection(number97) shouldEqual Seq(15)
    findVerticalReflection(number97) shouldEqual Seq.empty
  }

  test("No reflections should be here") {
    val grid = parse(List(
      ".#",
      "##"
    ))
    findVerticalReflection(grid) shouldEqual Seq.empty
    findHorizontalReflection(grid) shouldEqual Seq.empty
  }

  test("No reflections should be here either") {
    val grid = parse(List(
      "...",
      "..#",
      ".##"
    ))
    findVerticalReflection(grid) shouldEqual Seq.empty
    findHorizontalReflection(grid) shouldEqual Seq.empty
  }

  test("2 reflection here") {
    val grid = parse(List(
      "##",
      "##"
    ))
    findVerticalReflection(grid) shouldEqual Seq(1)
    findHorizontalReflection(grid) shouldEqual Seq(1)
  }
}
