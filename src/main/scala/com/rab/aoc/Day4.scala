package com.rab.aoc

object Day4 {
  case class Scratchcard(winningNumbers: Set[Int], numbersYouHave: Set[Int]) {
    val matchCount: Int = numbersYouHave.intersect(winningNumbers).size
  }

  private def stringToSet(s: String): Set[Int] = {
    s.split(" ").filter(!_.isBlank).map(_.toInt).toSet
  }

  private val lineRegex = "^Card\\s+\\d+:\\s+([\\d\\s]+)\\|([\\d\\s]+)".r

  def parse(line: String): Scratchcard = {
    lineRegex.findFirstMatchIn(line).map { m =>
      Scratchcard(stringToSet(m.group(1)), stringToSet(m.group(2)))
    }.get
  }

  def solveDay4Part1(input: List[String]): Int = {
    input.map(parse).map(c => math.pow(2, c.matchCount-1).intValue).sum
  }

  def solveDay4Part2(input: List[String]): Long = {
    val numberOfAwardedCards = input.map(parse).map(_.matchCount)
    numberOfAwardedCards
      .reverse
      .zipWithIndex
      .foldLeft(List.fill(numberOfAwardedCards.length)(0L)) { (acc, e) =>
        val (currCount, i) = e
        if currCount <= 0 then acc.updated(i, 1)
        else {
          val newScore = acc.slice(i-currCount, i).sum
          acc.updated(i, newScore + 1)
        }
      }
      .sum
  }
}
