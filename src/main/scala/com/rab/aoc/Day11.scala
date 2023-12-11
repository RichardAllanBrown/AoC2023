package com.rab.aoc

import com.rab.aoc.helpers.LongCoordinate

object Day11 {
  def parseInput(lines: List[String]): Seq[LongCoordinate] = {
    lines.zipWithIndex.flatMap { case (l, y) =>
      l.zipWithIndex.collect { case (c, x) if c == '#' => LongCoordinate(x, y) }
    }
  }

  private def getEmpty(points: Seq[LongCoordinate])(f: LongCoordinate => Long): Set[Long] = {
    val values = points.map(f(_)).toSet
    (values.min to values.max).filterNot(values.contains).toSet
  }

  def expandUniverse(coordinates: Seq[LongCoordinate], replaceWith: Int = 2): Seq[LongCoordinate] = {
    val emptyXs = getEmpty(coordinates)(_.x)
    val emptyYs = getEmpty(coordinates)(_.y)
    val factor = replaceWith - 1
    coordinates.map {
      case LongCoordinate(x, y) => LongCoordinate(
        x + (emptyXs.count(_ < x) * factor),
        y + (emptyYs.count(_ < y) * factor)
      )
    }
  }

  def computeDistances(coordinates: Seq[LongCoordinate]): Long = {
    coordinates.combinations(2)
      .map(pair => pair.head.manhattanDistance(pair(1)))
      .sum
  }

  def solvePart1(lines: List[String]): Long = {
    val coordinates = parseInput(lines)
    val expandedCoords = expandUniverse(coordinates)
    computeDistances(expandedCoords)
  }

  def solvePart2(lines: List[String]): Long = {
    val coordinates = parseInput(lines)
    val expandedCoords = expandUniverse(coordinates, replaceWith = 1000000)
    computeDistances(expandedCoords)
  }
}
