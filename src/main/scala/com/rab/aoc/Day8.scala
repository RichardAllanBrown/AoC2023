package com.rab.aoc

import scala.annotation.tailrec

object Day8 {
  type Directions = LazyList[Char]
  type Network = Map[String, (String, String)]

  def countSteps(d: Directions, network: Network): Int = {
    @tailrec
    def helper(directions: LazyList[Char], visited: Seq[String]): Seq[String] = {
      val currentPos = visited.last
      if currentPos == "ZZZ" then visited
      else {
        val (left, right) = network(currentPos)
        val newPos = if directions.head == 'L' then left else right
        helper(directions.tail, visited :+ newPos)
      }
    }

    helper(d, Seq("AAA")).length - 1
  }

  def solvePart1(lines: List[String]): Int = {
    val (directions, network) = parseInput(lines)
    countSteps(directions, network)
  }

  private val networkMapRegex = "([A-Z1-9]{3}) = \\(([A-Z1-9]{3}), ([A-Z1-9]{3})\\)".r
  private def parseInput(lines: List[String]) = {
    val directions = LazyList.continually(lines.head.toCharArray).flatten
    val network = lines.drop(2).map(l => {
      val matcher = networkMapRegex.findFirstMatchIn(l).get
      val source = matcher.group(1)
      val left = matcher.group(2)
      val right = matcher.group(3)
      source -> (left, right)
    }).toMap
    (directions, network)
  }

  def countGhostSteps(d: Directions, network: Network): Int = {
    // todo: improve efficiency, probably by computing the step count that puts you on a terminal
    // space for each starting point and finding when they meet together
    @tailrec
    def helper(directions: LazyList[Char], visited: Seq[Seq[String]]): Seq[Seq[String]] = {
      val currentPositions = visited.last
      if currentPositions.forall(_.endsWith("Z")) then visited
      else {
        val newPositions = currentPositions.map(p => {
          val (left, right) = network(p)
          if directions.head == 'L' then left else right
        })
        helper(directions.tail, visited :+ newPositions)
      }
    }

    helper(d, Seq(network.keys.filter(_.endsWith("A")).toSeq)).length - 1
  }

  def solvePart2(lines: List[String]): Int = {
    val (directions, network) = parseInput(lines)
    countGhostSteps(directions, network)
  }
}
