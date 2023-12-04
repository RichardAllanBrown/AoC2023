package com.rab.aoc

object Day1 {
  def findCalibrationValue(s: String): Int = {
    val digits = s.toCharArray.filter(_.isDigit).map(_.asDigit)
    digits.head*10 + digits.last
  }

  def solvePart1Puzzle(input: List[String]): Int = {
    input.map(findCalibrationValue).sum
  }

  def convertTypedDigits(s: String): String = {
    s.replace("one", "o1e")
      .replace("two", "t2o")
      .replace("three", "th3ee")
      .replace("four", "f4ur")
      .replace("five", "f5ve")
      .replace("six", "s6x")
      .replace("seven", "se7en")
      .replace("eight", "ei8ht")
      .replace("nine", "n9ne")
  }

  def solvePart2Puzzle(input: List[String]): Int = {
    input.map(convertTypedDigits).map(findCalibrationValue).sum
  }
}
