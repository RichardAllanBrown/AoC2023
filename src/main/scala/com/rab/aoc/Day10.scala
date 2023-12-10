package com.rab.aoc

import com.rab.aoc.helpers.{Coordinate, Grid}

import scala.annotation.tailrec

object Day10 {
  def coordsWherePipeLeads(g: Grid[Char])(c: Coordinate): Seq[Coordinate] = {
    val coords = g.get(c) match
      case '|' => Seq(c.up, c.down)
      case '-' => Seq(c.left, c.right)
      case 'L' => Seq(c.up, c.right)
      case 'J' => Seq(c.up, c.left)
      case '7' => Seq(c.left, c.down)
      case 'F' => Seq(c.right, c.down)
      case _ => Seq.empty
    coords.filter(g.containsPoint)
  }

  def followLoop(g: Grid[Char]): Seq[Coordinate] = {
    @tailrec
    def helper(visited: Seq[Coordinate]): Seq[Coordinate] = {
      val nextCoord = coordsWherePipeLeads(g)(visited.last) match
        case Seq(a, b) if a == visited(visited.length - 2) => b
        case Seq(a, b) if b == visited(visited.length - 2) => a
        case _ => throw new RuntimeException("Path doesn't loop")
      if nextCoord == visited.head then visited else helper(visited :+ nextCoord)
    }
    val startCoord = g.findFirstPoint(_ == 'S').get
    val leadsTo = startCoord.cardinalNeighbours
      .filter(n => coordsWherePipeLeads(g)(n).contains(startCoord))
    helper(Seq(startCoord, leadsTo.head))
  }

  def solvePart1(lines: List[String]): Int = {
    val grid = Grid.fromInput(lines)
    val path = followLoop(grid)
    path.length / 2
  }
}
