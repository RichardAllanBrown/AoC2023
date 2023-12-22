package com.rab.aoc

import com.rab.aoc.Day21.Garden.FlowerBed
import com.rab.aoc.helpers.{Coordinate, Grid}

import scala.collection.immutable

object Day21 {
  enum Garden:
    case Empty, FlowerBed, Elf

  def gardenToString(g: Garden): String = g match
    case Garden.Empty => "."
    case Garden.FlowerBed => "#"
    case Garden.Elf => "S"

  def parseInput(lines: List[String]): Grid[Garden] = {
    Grid.fromInput(lines).map {
      case '.' => Garden.Empty
      case '#' => Garden.FlowerBed
      case 'S' => Garden.Elf
    }
  }

  case class VisitedTracker(visited: Map[Int, Set[Coordinate]]) {
    def hasVisitedEarlier(step: Int, c: Coordinate): Boolean = {
      visited.filter(_._1 <= step).exists(p => p._2.contains(c))
    }
    def withVisit(step: Int, c: Coordinate): VisitedTracker = {
      val set = visited.getOrElse(step, Set.empty) + c
      copy(visited = visited.updated(step, set))
    }
    def all: Set[Coordinate] = visited.flatMap(_._2).toSet
  }

  def getAllDestinationsAfterSteps(grid: Grid[Garden], maxSteps: Int): Set[Coordinate] = {
    def helper(step: Int, currentPlace: Coordinate, tracker: VisitedTracker): VisitedTracker = {
      if step == maxSteps then tracker
      else {
        val newSpots = grid.getCardinalNeighbouringPoints(currentPlace)
          .filterNot(grid.get(_) == Garden.FlowerBed)
          .flatMap(grid.getCardinalNeighbouringPoints)
          .filterNot(grid.get(_) == Garden.FlowerBed)
          .toSet
          .filterNot(tracker.hasVisitedEarlier(step + 2, _))
        newSpots.foldLeft(tracker)((t, ns) => {
          val newVisited = t.withVisit(step + 2, ns)
          helper(step + 2, ns, newVisited)
        })
      }
    }
    val startingPoint = grid.findFirstPoint(_ == Garden.Elf).get
    val tracker = helper(0, startingPoint, VisitedTracker(Map.empty))
    tracker.all
  }

  def solvePart1(lines: List[String]): Int = {
    val grid = parseInput(lines)
    val destinations = getAllDestinationsAfterSteps(grid, 64)
    Grid.print(grid.mapi((v, i) => if destinations.contains(i) && v != Garden.Elf then "O" else gardenToString(v)), identity)
    destinations.size
  }
}
