package com.rab.aoc

import com.rab.aoc.Day2._

class Day2Test extends UnitSpec {
  test("Parsing a single game line") {
    val result = parseGame("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    result shouldEqual Game(1, List(Hand(4, 0, 3), Hand(1, 2, 6), Hand(0, 2, 0)))
  }

  test("The example computes the expected answer") {
    val input = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
    solvePart1Puzzle(input) shouldEqual 8
  }
}
