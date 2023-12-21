package com.rab.aoc

class Day20Test extends UnitSpec {
  test("Handles simple example") {
    val simpleInput = List(
      "broadcaster -> a, b, c",
      "%a -> b",
      "%b -> c",
      "%c -> inv",
      "&inv -> a"
    )

    Day20.solvePart1(simpleInput) shouldEqual 32000000L
  }
}
