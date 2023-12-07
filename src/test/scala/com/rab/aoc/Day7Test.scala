package com.rab.aoc

import com.rab.aoc.Day7._

class Day7Test extends UnitSpec {
  test("Can order hands correctly") {
    val result = Seq(parseHand("KKKK3 2"), parseHand("AAAAA 5"), parseHand("QQQQQ 4")).sorted
    result shouldEqual Seq(parseHand("KKKK3 2"), parseHand("QQQQQ 4"), parseHand("AAAAA 5"))
  }

  val example = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )

  test("Correct answer for example") {
     solvePart1(example) shouldEqual 6440
  }

  test("Correct answer with jokers for example") {
    solvePart2(example) shouldEqual 5905
  }
}
