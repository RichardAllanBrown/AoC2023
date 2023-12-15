package com.rab.aoc

object Day15 {
  private def computeHash(s: String): Int = {
    def processChar(v: Int, c: Char): Int = {
      val step1 = v + c.toInt
      val step2 = step1 * 17
      step2 % 256
    }
    s.toCharArray.foldLeft(0)(processChar)
  }

  def solvePart1(input: List[String]) = {
    input.flatMap(_.split(',')).map(computeHash).sum
  }
}
