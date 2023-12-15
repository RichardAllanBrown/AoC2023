package com.rab.aoc

import com.rab.aoc.helpers.{Coordinate, Grid}

object Day13 {
  def findVerticalReflection(rocks: Grid[Boolean]): Option[Int] = {
    def hasReflectionAt(v: Int): Boolean = {
      val (left, right) = rocks.findPoints(identity).partition(_.x < v)
      val range = math.min(rocks.width - v, v)
      val relevantLeft = left.filter(v - range <= _.x)
      val relevantRight = right.filter(_.x <= v + range)
      val mirroredLeftSet = relevantLeft.map(c => c.copy(x = v*2 - c.x - 1)).toSet
      val rightSet = relevantRight.toSet

      mirroredLeftSet == rightSet
    }
    (1 until rocks.width).find(hasReflectionAt)
  }

  def findHorizontalReflection(rocks: Grid[Boolean]): Option[Int] = {
    def hasReflectionAt(h: Int): Boolean = {
      val (top, bottom) = rocks.findPoints(identity).partition(_.y < h)
      val range = math.min(rocks.height - h, h)
      val relevantTop = top.filter(h - range <= _.y)
      val relevantBottom = bottom.filter(_.y <= h + range)
      val mirroredTopSet = relevantTop.map(c => c.copy(y = h*2 - c.y - 1)).toSet
      val bottomSet = relevantBottom.toSet

      mirroredTopSet == bottomSet
    }
    (1 until rocks.height).find(hasReflectionAt)
  }

  def getReflectionValue(rocks: Grid[Boolean]): Int = {
    findVerticalReflection(rocks)
      .orElse(findHorizontalReflection(rocks).map(_*100))
      .get
  }

  private def printIt(rocks: Grid[Boolean]) = {
    rocks.values.sliding(rocks.width, rocks.width).foreach { r =>
      println(r.map(if _ then '#' else '.').mkString)
    }
  }

  def parse(l: List[String]): Grid[Boolean] = Grid.fromInput(l).map(_ == '#')

  def parseMany(l: List[String]): Seq[Grid[Boolean]] = {
    val nextEmptyLine = l.indexWhere(_.isEmpty)
    if nextEmptyLine < 0 then Seq(parse(l))
    else parse(l.take(nextEmptyLine)) +: parseMany(l.drop(nextEmptyLine + 1))
  }

  def solvePart1(lines: List[String]): Int = {
    parseMany(lines).map(getReflectionValue).sum
  }
}
