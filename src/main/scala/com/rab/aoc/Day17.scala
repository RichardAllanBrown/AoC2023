package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 {
  private def parse(lines: List[String]) = Grid.fromInput(lines).map(_.asDigit)

  def findCheapestPath(minLinearMove: Int, maxLinearMove: Int)(grid: Grid[Int]): Int = {
    val start = Coordinate(0, 0)
    val target = Coordinate(grid.width - 1, grid.height - 1)

    def getPathCost(p: Seq[Coordinate]): Int = p.drop(1).map(grid.get).sum

    implicit object PathCostOrdering extends Ordering[Seq[Coordinate]] {
      def getPriority(a: Seq[Coordinate]): Int = {
        Int.MaxValue - getPathCost(a) - target.manhattanDistance(a.last)
      }
      override def compare(x: Seq[Coordinate], y: Seq[Coordinate]): Int = {
        getPriority(x).compare(getPriority(y))
      }
    }

    val paths = mutable.PriorityQueue.empty(PathCostOrdering)
    paths.addOne(Seq(start))
    val cheapestRoutesTo = mutable.Map.empty[(Coordinate, Orientation), Int]
    cheapestRoutesTo.addOne((start, Horizontal) -> 0)
    cheapestRoutesTo.addOne((start, Vertical) -> 0)

    def getOrientation(path: Seq[Coordinate]): Orientation = {
      val curr = path.last
      val prev = path.lift(path.length - 2)
      prev match
        case Some(curr.left) => Horizontal
        case Some(curr.right) => Horizontal
        case Some(curr.up) => Vertical
        case Some(curr.down) => Vertical
        case _ => throw new RuntimeException("Developer fail!")
    }

    def getPossibleNextPaths(path: Seq[Coordinate]): Seq[Seq[Coordinate]] = {
      val curr = path.last
      val prev = path.lift(path.length - 2)
      val dirs = prev match
        case Some(curr.left) => Seq(Up, Down)
        case Some(curr.right) => Seq(Up, Down)
        case Some(curr.up) => Seq(Left, Right)
        case Some(curr.down) => Seq(Left, Right)
        case None => Seq(Up, Down, Left, Right)
        case _ => throw new RuntimeException("Dev error!")

      def allowedMove(m: Seq[Coordinate]): Boolean = m.forall { c =>
        grid.containsPoint(c) && !path.contains(c)
      }

      dirs.flatMap(d => {
        val move3 = (1 until 4).map(curr.moveN(_, d))
        val move2 = (1 until 3).map(curr.moveN(_, d))
        val move1 = Seq(curr.moveOne(d))

        if allowedMove(move3) then Seq(path ++ move1, path ++ move2, path ++ move3)
        else if allowedMove(move2) then Seq(path ++ move1, path ++ move2)
        else if allowedMove(move1) then Seq(path ++ move1)
        else Seq.empty
      })
    }

    @tailrec
    def stepIt(): Unit = {
      if paths.isEmpty then ()
      else {
        val cheapestPath = paths.dequeue()
        if cheapestPath.last == target then ()
        else {
          val nextValidOptions = getPossibleNextPaths(cheapestPath)
            .map(n => (n, getPathCost(n), getOrientation(n)))
            .filter((path, cost, orientation) => {
              cost < cheapestRoutesTo.getOrElse((path.last, orientation), Int.MaxValue)
            })

          nextValidOptions.foreach { (path, cost, orientation) =>
            if (cost < cheapestRoutesTo.getOrElse((path.last, orientation), Int.MaxValue)) {
              cheapestRoutesTo.update((path.last, orientation), cost)
            }
            paths.addOne(path)
          }

          stepIt()
        }
      }
    }

    stepIt()
    math.min(
      cheapestRoutesTo.getOrElse((target, Vertical), Int.MaxValue),
      cheapestRoutesTo.getOrElse((target, Horizontal), Int.MaxValue)
    )
  }

  def solvePart1(lines: List[String]): Int = {
    val map = parse(lines)
    findCheapestPath(1, 3)(map)
  }

  def solvePart2(lines: List[String]): Int = {
    val map = parse(lines)
    findCheapestPath(4, 10)(map)
  }
}
