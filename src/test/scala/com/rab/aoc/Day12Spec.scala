package com.rab.aoc

import com.rab.aoc.Day12.SpringRow
import com.rab.aoc.Day12.SpringRow.valid

class Day12Spec extends UnitSpec {
  test("Validity is as expected") {
    valid(SpringRow(".#...#....###.".toCharArray, Seq(1, 1, 3))) shouldBe true
    valid(SpringRow(".###.##....#".toCharArray, Seq(3, 2, 1))) shouldBe true
  }
}
