package com.rab.aoc.helpers

import com.rab.aoc.UnitSpec

class RangeTest extends UnitSpec {
  test("splitRangeLessThan returns expected values") {
    Range(5, 15).splitRangeLessThan(9) shouldEqual (Some(Range(5, 8)), Some(Range(9, 15)))
    Range(5, 15).splitRangeLessThan(3) shouldEqual (None, Some(Range(5, 15)))
    Range(5, 15).splitRangeLessThan(24) shouldEqual (Some(Range(5, 15)), None)
    Range(5, 15).splitRangeLessThan(5) shouldEqual (None, Some(Range(5, 15)))
    Range(5, 15).splitRangeLessThan(15) shouldEqual (Some(Range(5, 14)), Some(Range(15, 15)))
  }

  test("splitRangeMoreThan returns expected values") {
    Range(5, 15).splitRangeMoreThan(9) shouldEqual(Some(Range(5, 9)), Some(Range(10, 15)))
    Range(5, 15).splitRangeMoreThan(3) shouldEqual(None, Some(Range(5, 15)))
    Range(5, 15).splitRangeMoreThan(24) shouldEqual(Some(Range(5, 15)), None)
    Range(5, 15).splitRangeMoreThan(5) shouldEqual(Some(Range(5, 5)), Some(Range(6, 15)))
    Range(5, 15).splitRangeMoreThan(15) shouldEqual(Some(Range(5, 15)), None)
  }

  test("Intersection is true for overlapping ranges") {
    Range(5, 10).intersectsWith(Range(1, 15)) shouldBe true
    Range(1, 15).intersectsWith(Range(5, 10)) shouldBe true
  }

  test("Length computes correctly") {
    Range(5, 15).length shouldEqual 11
    Range(1, 1).length shouldEqual 1
  }
}
