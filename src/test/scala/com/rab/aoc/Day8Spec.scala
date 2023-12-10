package com.rab.aoc

import com.rab.aoc.Day8._

class Day8Spec extends UnitSpec {
  test("Solve the first example") {
    val lines = List(
      "RL",
      "",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    )

    solvePart1(lines) shouldEqual 2
  }

  test("Solve the second example") {
    val lines = List(
      "LLR",
      "",
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    )

    solvePart1(lines) shouldEqual 6
  }

  test("Solve the part 2 example") {
    val lines = List(
      "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    )

    solvePart2(lines) shouldEqual 6
  }
}