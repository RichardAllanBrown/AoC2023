package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 {
  private def parse(lines: List[String]) = Grid.fromInput(lines).map(_.toInt)

  def findCheapestPathBad(grid: Grid[Int]): Int = {
    val start = Coordinate(0, 0)
    val target = Coordinate(grid.width - 1, grid.height - 1)

    implicit object PathCostOrdering extends Ordering[Seq[Coordinate]] {
      def getCost(a: Seq[Coordinate]): Int = {
        a.drop(1).map(grid.get).sum - target.manhattanDistance(a.last)*10
      }
      override def compare(x: Seq[Coordinate], y: Seq[Coordinate]): Int = {
        getCost(y).compare(getCost(x))
      }
    }

    val paths = mutable.PriorityQueue.empty(PathCostOrdering)
    paths.addOne(Seq(start))

    @tailrec
    def stepIt(): Seq[Coordinate] = {
      val cheapestPath = paths.dequeue()
      if cheapestPath.last == target then cheapestPath else {
        val nextValidOptions = grid.getCardinalNeighbouringPoints(cheapestPath.last)
          .filterNot(cheapestPath.contains)
          .filter(n => {
            val last4Nodes = cheapestPath.reverse.take(3) :+ n
            last4Nodes.length < 4 ||
              (1 < last4Nodes.map(_.x).distinct.length && 1 < last4Nodes.map(_.y).distinct.length)
          })

        paths.addAll(nextValidOptions.map(cheapestPath :+ _))
        stepIt()
      }
    }

    val cheapestPath = stepIt()
    PathCostOrdering.getCost(cheapestPath)
  }

  def solvePart1(lines: List[String]): Int = {
    val map = parse(lines)
    findCheapestPathBad(map)
  }
}
