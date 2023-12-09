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

  def getEndPositions(directions: Seq[Char], network: Network)(start: String): LazyList[Int] = {
    LazyList.iterate((0, start))(args => {
      val (stepCount, currPos) = args
      val currIndex = if stepCount == 0 then 0 else stepCount % directions.length
      val (left, right) = network(currPos)
      val nextPos = if directions(currIndex) == 'L' then left else right
      (stepCount + 1, nextPos)
    }).filter(s => s._2.endsWith("Z")).map(_._1)
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  def solvePart2(lines: List[String]): BigInt = {
    val (_, network) = parseInput(lines)
    val directions = lines.head.toCharArray.toSeq
    val startingPositions = network.keys.filter(_.endsWith("A")).toSeq
    println(s"There are ${startingPositions.length} starting positions")
    val compEnds = getEndPositions(directions, network)
    val repeatLengths = startingPositions.map(compEnds).map(i => BigInt(i(3)-i(2)))
    println(s"The cycle lengths are $repeatLengths")
    lcm(repeatLengths)
  }
}
