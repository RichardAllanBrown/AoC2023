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

  private val firstGrid = parse(exampleLines.take(7))
  private val secondGrid = parse(exampleLines.drop(8))

  test("Can handle first example correctly") {
    findVerticalReflection(firstGrid) shouldEqual Some(5)
    findHorizontalReflection(firstGrid) shouldEqual None
  }

  test("Can handle second example correctly") {
    findHorizontalReflection(secondGrid) shouldEqual Some(4)
    findVerticalReflection(secondGrid) shouldEqual None
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
    findHorizontalReflection(number97) shouldEqual Some(15)
    findVerticalReflection(number97) shouldEqual None
  }

  test("No reflections should be here") {
    val grid = parse(List(
      ".#",
      "##"
    ))
    findVerticalReflection(grid) shouldEqual None
    findHorizontalReflection(grid) shouldEqual None
  }

  test("No reflections should be here either") {
    val grid = parse(List(
      "...",
      "..#",
      ".##"
    ))
    findVerticalReflection(grid) shouldEqual None
    findHorizontalReflection(grid) shouldEqual None
  }

  test("2 reflection here") {
    val grid = parse(List(
      "##",
      "##"
    ))
    findVerticalReflection(grid) shouldEqual Some(1)
    findHorizontalReflection(grid) shouldEqual Some(1)
  }

  test("More complex and fuller example is solved for part 1") {
    val lines = """#.##..##.
                 |..#.##.#.
                 |##......#
                 |##......#
                 |..#.##.#.
                 |..##..##.
                 |#.#.##.#.
                 |
                 |#...##..#
                 |#....#..#
                 |..##..###
                 |#####.##.
                 |#####.##.
                 |..##..###
                 |#....#..#
                 |
                 |.#.##.#.#
                 |.##..##..
                 |.#.##.#..
                 |#......##
                 |#......##
                 |.#.##.#..
                 |.##..##.#
                 |
                 |#..#....#
                 |###..##..
                 |.##.#####
                 |.##.#####
                 |###..##..
                 |#..#....#
                 |#..##...#
                 |
                 |#.##..##.
                 |..#.##.#.
                 |##..#...#
                 |##...#..#
                 |..#.##.#.
                 |..##..##.
                 |#.#.##.#.""".stripMargin.linesIterator.toList

    solvePart1(lines) shouldEqual 709
  }
}
