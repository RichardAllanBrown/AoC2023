package com.rab.aoc

import com.rab.aoc.Day5._
import com.rab.aoc.helpers.Range

class Day5Test extends UnitSpec {
  test("Ranged addition with ranges wholly inside") {
    val (newVal, remaining) = RangedAddition(Range(1, 100), 3).applyTo(Range(5, 10))
    newVal shouldEqual Some(Range(8, 13))
    remaining shouldEqual Seq.empty
  }

  test("Ranged addition with ranges overlapping start") {
    val (newVal, remaining) = RangedAddition(Range(7, 100), 3).applyTo(Range(5, 10))
    newVal shouldEqual Some(Range(10, 13))
    remaining shouldEqual Seq(Range(5, 6))
  }

  test("Ranged addition with ranges overlapping end") {
    val (newVal, remaining) = RangedAddition(Range(1, 10), 3).applyTo(Range(5, 15))
    newVal shouldEqual Some(Range(8, 13))
    remaining shouldEqual Seq(Range(11, 15))
  }

  test("Ranged addition with ranges overlapping entire add func") {
    val (newVal, remaining) = RangedAddition(Range(5, 10), 3).applyTo(Range(1, 15))
    newVal shouldEqual Some(Range(8, 13))
    remaining shouldEqual Seq(Range(1, 4), Range(11, 15))
  }
}
