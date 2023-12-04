package com.rab.aoc

import scala.util.matching.Regex

object Day2 {
  case class Hand(red: Int, green: Int, blue: Int) {
    def isPossible: Boolean = red <= 12 && green <= 13 && blue <= 14
  }
  case object Hand {
    val empty: Hand = Hand(0, 0, 0)
  }
  case class Game(id: Int, hands: List[Hand]) {
    def isPossible: Boolean = hands.forall(_.isPossible)
    def minPowerNeeded: Int = hands.map(_.red).max * hands.map(_.blue).max * hands.map(_.green).max
  }

  private val handRegex = "(\\d+) (red|blue|green)".r
  def parseHand(s: String): Hand = {
    handRegex.findAllMatchIn(s).foldRight(Hand.empty)((m, h) => {
      val count = m.group(1).toInt
      m.group(2) match
        case "red" => h.copy(red = count)
        case "blue" => h.copy(blue = count)
        case "green" => h.copy(green = count)
        case _ => throw new RuntimeException(s"Unknown color in $s")
    })
  }

  val lineRegex: Regex = "Game (\\d+): (.*)".r
  def parseGame(s: String): Game = {
    val parsedLine = for {
      line <- lineRegex.findAllMatchIn(s)
      gameId = line.group(1).toInt
      hands = line.group(2).split(';').map(parseHand).toList
    } yield {
      Game(gameId, hands)
    }
    parsedLine.toList.head
  }

  def solvePart1Puzzle(input: List[String]): Int = {
    input.map(parseGame).filter(_.isPossible).map(_.id).sum
  }

  def solvePart2Puzzle(input: List[String]): Int = {
    input.map(parseGame).map(_.minPowerNeeded).sum
  }
}
