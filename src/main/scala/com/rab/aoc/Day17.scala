package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 {
  private def parse(lines: List[String]) = Grid.fromInput(lines).map(_.toInt)

  def findCheapestPathBad(grid: Grid[Int]): Int = {
    val start = Coordinate(0, 0)
    val target = Coordinate(grid.width - 1, grid.height - 1)

    def getPathCost(p: Seq[Coordinate]): Int = p.drop(1).map(grid.get).sum

    implicit object PathCostOrdering extends Ordering[Seq[Coordinate]] {
      def getCost(a: Seq[Coordinate]): Int = {
        Int.MaxValue - getPathCost(a) - target.manhattanDistance(a.last)*10
      }
      override def compare(x: Seq[Coordinate], y: Seq[Coordinate]): Int = {
        getCost(x).compare(getCost(y))
      }
    }

    val paths = mutable.PriorityQueue.empty(PathCostOrdering)
    paths.addOne(Seq(start))
    val cheapestRoutesTo = mutable.Map.empty[Coordinate, Int]
    cheapestRoutesTo.addOne(start -> 0)

    @tailrec
    def stepIt(): Seq[Coordinate] = {
      val cheapestPath = paths.dequeue()
      if cheapestPath.last == target then cheapestPath else {
        val nextValidOptions = grid.getCardinalNeighbouringPoints(cheapestPath.last)
          .map(n => cheapestPath :+ n)
          .map(n => (n, getPathCost(n)))
          // If there is a cheaper way of getting here, don't bother
          .filter((path, cost) => {
            cost < cheapestRoutesTo.getOrElse(path.last, Int.MaxValue)
          })
          // Ensure we don't go straight for more than 3 spaces
          .filter((path, _) => {
            val last4Nodes = path.reverse.take(4)
            last4Nodes.length < 4 ||
              (1 < last4Nodes.map(_.x).distinct.length && 1 < last4Nodes.map(_.y).distinct.length)
          })

        nextValidOptions.foreach { (path, cost) =>
          cheapestRoutesTo.addOne(path.last -> cost)
          paths.addOne(path)
        }

        stepIt()
      }
    }

    val cheapestPath = stepIt()
    getPathCost(cheapestPath)
  }

  def solvePart1(lines: List[String]): Int = {
    val map = parse(lines)
    findCheapestPathBad(map)
  }
}
