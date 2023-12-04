package com.rab.aoc

import Day1.findCalibrationValue

class Day1Test extends UnitSpec {
  test("Day1 can parse the example lines") {
    findCalibrationValue("1abc2") shouldEqual 12
    findCalibrationValue("pqr3stu8vwx") shouldEqual 38
    findCalibrationValue("a1b2c3d4e5f") shouldEqual 15
    findCalibrationValue("treb7uchet") shouldEqual 77
  }
}
