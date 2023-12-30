package com.rab.aoc

import com.rab.aoc.helpers.Grid

object Day13 {
  def findVerticalReflection(rocks: Grid[Boolean], allowForSmudge: Boolean): Option[Int] = {
    val rockCoords = rocks.findPoints(identity)

    def countDiffsAt(v: Int): Int = {
      LazyList.iterate((v - 1, v))(a => (a._1 - 1, a._2 + 1))
        .takeWhile(a => 0 <= a._1 && a._2 < rocks.width)
        .map(p => {
          val (lCol, rCol) = p
          val leftColRocks = rockCoords.filter(_.x == lCol).map(_.y).toSet
          val rightColRocks = rockCoords.filter(_.x == rCol).map(_.y).toSet
          leftColRocks.diff(rightColRocks).size + rightColRocks.diff(leftColRocks).size
        })
        .sum
    }

    (1 until rocks.width).find(w => countDiffsAt(w) == (if allowForSmudge then 1 else 0))
  }

  def findHorizontalReflection(rocks: Grid[Boolean], allowForSmudge: Boolean): Option[Int] = {
    val rotated = rocks.rotateClockwise
    findVerticalReflection(rotated, allowForSmudge).map(rotated.width - _)
  }

  private def getReflectionValue(rocks: Grid[Boolean], allowForSmudge: Boolean): Int = {
    findVerticalReflection(rocks, allowForSmudge)
      .orElse(findHorizontalReflection(rocks, allowForSmudge).map(_*100))
      .getOrElse(0)
  }

  private def printIt(rocks: Grid[Boolean]): Unit = {
    Grid.print(rocks, if _ then "#" else ".")
  }

  def parse(l: List[String]): Grid[Boolean] = Grid.fromInput(l).map(_ == '#')

  def parseMany(l: List[String]): Seq[Grid[Boolean]] = {
    val nextEmptyLine = l.indexWhere(_.isEmpty)
    if nextEmptyLine < 0 then Seq(parse(l))
    else parse(l.take(nextEmptyLine)) +: parseMany(l.drop(nextEmptyLine + 1))
  }

  def solvePart1(lines: List[String]): Int = {
    val grids = parseMany(lines)
    println(s"Found ${grids.length} grids in input")
    grids.map(getReflectionValue(_, false)).sum
  }

  def solvePart2(lines: List[String]): Int = {
    val grids = parseMany(lines)
    grids.map(getReflectionValue(_, true)).sum
  }
}
