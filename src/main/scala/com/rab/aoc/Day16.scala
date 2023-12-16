package com.rab.aoc

import com.rab.aoc.helpers._

object Day16 {
  private type Ray = (Coordinate, Direction)

  private def findRayPaths(g: Grid[Char], ray: Ray, seen: Set[Ray]): Set[Ray] = {
    val (c, d) = ray
    if seen.contains(ray) || !g.containsPoint(c) then seen
    else {
      val value = g.get(c)
      val newDirections = (value, d) match {
        case ('.', dir) => Seq(dir)
        case ('/', Up) => Seq(Right)
        case ('/', Down) => Seq(Left)
        case ('/', Left) => Seq(Down)
        case ('/', Right) => Seq(Up)
        case ('\\', Up) => Seq(Left)
        case ('\\', Down) => Seq(Right)
        case ('\\', Left) => Seq(Up)
        case ('\\', Right) => Seq(Down)
        case ('-', dir) if dir == Up || dir == Down => Seq(Left, Right)
        case ('-', dir) => Seq(dir)
        case ('|', dir) if dir == Left || dir == Right => Seq(Up, Down)
        case ('|', dir) => Seq(dir)
        case _ => throw new RuntimeException(s"Match was not exhaustive for $ray and value $value")
      }
      val newSeens = seen + ray
      newDirections match {
        case Seq(a, b) =>
          // when the path splits we are depth first NOT breadth first
          val firstSeens = findRayPaths(g, c.moveOne(a) -> a, newSeens)
          findRayPaths(g, c.moveOne(b) -> b, newSeens ++ firstSeens)
        case Seq(a) => findRayPaths(g, c.moveOne(a) -> a, newSeens)
      }
    }
  }

  def solvePart1(lines: List[String]): Int = {
    val mirrorGrid = Grid.fromInput(lines)
    val visitedSpaces = findRayPaths(mirrorGrid, Coordinate(0, 0) -> Right, Set.empty)
    visitedSpaces.map(_._1).size
  }

  def solvePart2(lines: List[String]): Int = {
    val mirrorGrid = Grid.fromInput(lines)
    val allStartSpaces = Seq(
      (0 until mirrorGrid.height).map(Coordinate(0, _) -> Right),
      (0 until mirrorGrid.height).map(Coordinate(mirrorGrid.width - 1, _) -> Left),
      (0 until mirrorGrid.width).map(Coordinate(_, 0) -> Down),
      (0 until mirrorGrid.width).map(Coordinate(_, mirrorGrid.height - 1) -> Up)
    ).flatten
    allStartSpaces.map(findRayPaths(mirrorGrid, _, Set.empty).map(_._1).size).max
  }
}
