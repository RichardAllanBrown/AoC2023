package com.rab.aoc

object Day6 {
  def getBestTimeCount(time: Int, distanceToBeat: Long): Long = {
    val distances = (1 until time).map(speed => (time-speed).toLong*speed.toLong)
    distances.count(_ > distanceToBeat)
  }

  def solvePart1(lines: List[String]): Long = {
    val times = lines.head.drop(5).split(' ').filter(_.nonEmpty).map(_.toInt)
    val distances = lines(1).drop(9).split(' ').filter(_.nonEmpty).map(_.toLong)
    times.zip(distances)
      .map { case (time, distance) => getBestTimeCount(time, distance) }
      .product
  }

  def solvePart2(lines: List[String]): Long = {
    val time = lines.head.drop(5).filter(!_.isWhitespace).toInt
    val distance = lines(1).drop(9).filter(!_.isWhitespace).toLong
    getBestTimeCount(time, distance)
  }
}
