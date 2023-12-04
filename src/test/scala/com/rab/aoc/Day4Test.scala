package com.rab.aoc

import com.rab.aoc.Day4.Scratchcard

class Day4Test extends UnitSpec {
  test("Should be able to parse a scratchcard") {
    val card = Day4.parse("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    card shouldEqual Scratchcard(Set(41, 48, 83, 86, 17), Set(83, 86, 6, 31, 17 ,9, 48, 53))
    card.matchCount shouldEqual 4
  }

  test("Should compute the example correctly") {
    val input = List(
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    )
    Day4.solveDay4Part1(input) shouldEqual 13
  }
}
