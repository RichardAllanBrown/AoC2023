package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.::
import scala.annotation.tailrec
import scala.collection.mutable

object Day17 {
  private def parse(lines: List[String]) = Grid.fromInput(lines).map(_.asDigit)

  def findCheapestPathBad(grid: Grid[Int]): Int = {
    val start = Coordinate(0, 0)
    val target = Coordinate(grid.width - 1, grid.height - 1)

    def getPathCost(p: Seq[Coordinate]): Int = p.drop(1).map(grid.get).sum

    implicit object PathCostOrdering extends Ordering[Seq[Coordinate]] {
      def getCost(a: Seq[Coordinate]): Int = {
        Int.MaxValue - getPathCost(a) - target.manhattanDistance(a.last)
      }
      override def compare(x: Seq[Coordinate], y: Seq[Coordinate]): Int = {
        getCost(x).compare(getCost(y))
      }
    }

    val paths = mutable.PriorityQueue.empty(PathCostOrdering)
    paths.addOne(Seq(start))
    val cheapestRoutesTo = mutable.Map.empty[Coordinate, Int]
    cheapestRoutesTo.addOne(start -> 0)

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
            .map(n => (n, getPathCost(n)))
            .filter((path, cost) => {
              cost < cheapestRoutesTo.get(path.last).map(_ + 5).getOrElse(Int.MaxValue)
            })

          nextValidOptions.foreach { (path, cost) =>
            if (cost < cheapestRoutesTo.getOrElse(path.last, Int.MaxValue)) {
              cheapestRoutesTo.update(path.last, cost)
            }
            paths.addOne(path)
          }

          stepIt()
        }
      }
    }

    stepIt()
    cheapestRoutesTo(target)
  }

  def solvePart1(lines: List[String]): Int = {
    val map = parse(lines)
    findCheapestPathBad(map)
  }
}
