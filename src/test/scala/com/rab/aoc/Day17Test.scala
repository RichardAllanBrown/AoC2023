package com.rab.aoc

class Day17Test extends UnitSpec {
  test("Can solve a very simple example") {
    val example = List(
      "1199",
      "9119",
      "9911",
      "9991"
    )

    Day17.solvePart1(example) shouldEqual 6
  }

  test("Cannot have more than 4 in a row") {
    val example = List(
      "999999999",
      "111111111",
      "999999991"
    )

    Day17.solvePart1(example) shouldEqual 36
  }

  test("Can solve example") {
    val example = List(
      "2413432311323",
      "3215453535623",
      "3255245654254",
      "3446585845452",
      "4546657867536",
      "1438598798454",
      "4457876987766",
      "3637877979653",
      "4654967986887",
      "4564679986453",
      "1224686865563",
      "2546548887735",
      "4322674655533"
    )

    Day17.solvePart1(example) shouldEqual 102
  }
}
