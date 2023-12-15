package com.rab.aoc

import com.rab.aoc.helpers.{Coordinate, Grid}

object Day14 {
  sealed trait Stone
  case object Immovable extends Stone
  case object Rollable extends Stone

  type Platform = Grid[Option[Stone]]

  def printIt(p: Platform) = {
    p.map {
      case None => "."
      case Some(Immovable) => "#"
      case Some(Rollable) => "O"
    }
      .values
      .sliding(p.width, p.width)
      .map(_.mkString)
      .foreach(println)
  }

  def parse(lines: List[String]): Platform = {
    Grid.fromInput(lines).map {
      case '.' => None
      case '#' => Some(Immovable)
      case 'O' => Some(Rollable)
    }
  }

  def tiltUp(platform: Platform): Platform = {
    val sortedPoints = platform.findPoints(_.contains(Rollable)).filter(0 < _.y).sorted
    sortedPoints.foldLeft(platform)((p, c) => {
      val col = (0 until c.y).reverse.map(y => Coordinate(c.x, y))
      // if the whole col is blocked, we don't move
      if col.forall(p.get(_).isDefined) then p else {
        val newPlace = col
          .find(possiblePlace => p.get(possiblePlace).isDefined)
          .map(_.down)
          .getOrElse(Coordinate(c.x, 0))
        p.setValue(c, None).setValue(newPlace, Some(Rollable))
      }
    })
  }

  def computeTopLoad(p: Platform): Int = {
    p.findPoints(_.contains(Rollable))
      .map(p.height - _.y)
      .sum
  }

  def solvePart1(lines: List[String]): Int = {
    import scala.util.chaining._
    lines.pipe(parse).pipe(tiltUp).pipe(computeTopLoad)
  }
}
