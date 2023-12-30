package com.rab.aoc

import com.rab.aoc.Day13._

class Day13Test extends UnitSpec {
  private val exampleLines = List(
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
    findVerticalReflection(firstGrid, allowForSmudge = false) shouldEqual Some(5)
    findHorizontalReflection(firstGrid, allowForSmudge = false) shouldEqual None
  }

  test("Can handle second example correctly") {
    findHorizontalReflection(secondGrid, allowForSmudge = false) shouldEqual Some(4)
    findVerticalReflection(secondGrid, allowForSmudge = false) shouldEqual None
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
    findHorizontalReflection(number97, allowForSmudge = false) shouldEqual Some(15)
    findVerticalReflection(number97, allowForSmudge = false) shouldEqual None
  }

  test("No reflections should be here") {
    val grid = parse(List(
      ".#",
      "##"
    ))
    findVerticalReflection(grid, allowForSmudge = false) shouldEqual None
    findHorizontalReflection(grid, allowForSmudge = false) shouldEqual None
  }

  test("No reflections should be here either") {
    val grid = parse(List(
      "...",
      "..#",
      ".##"
    ))
    findVerticalReflection(grid, allowForSmudge = false) shouldEqual None
    findHorizontalReflection(grid, allowForSmudge = false) shouldEqual None
  }

  test("2 reflection here") {
    val grid = parse(List(
      "##",
      "##"
    ))
    findVerticalReflection(grid, allowForSmudge = false) shouldEqual Some(1)
    findHorizontalReflection(grid, allowForSmudge = false) shouldEqual Some(1)
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
    solvePart2(lines) shouldEqual 1400
  }
}
