package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.annotation.tailrec

object Day23 {
  private val arrows = Map(
    Up -> '^',
    Down -> 'v',
    Left -> '<',
    Right -> '>'
  )
  private val allArrows = arrows.values.toSeq
  private val wall = '#'

  type SegmentAndLength = ((Coordinate, Coordinate), Int)
  type SegmentMap = Map[(Coordinate, Coordinate), Int]

  def buildPathGraph(grid: Grid[Char], start: Coordinate, target: Coordinate): SegmentMap = {
    def findPathSegments(segmentsLeft: Seq[(Coordinate, Direction)]): Seq[SegmentAndLength] = {
      if segmentsLeft.isEmpty then Seq.empty else {
        val (loc, dir) = segmentsLeft.head
        val nextSegment = walkToSplit(Seq(loc, loc.moveOne(dir)), dir)
        val ((segStart, segEnd), dist) = nextSegment
        val newToDo = if segEnd != target
          then getNextDirs(grid, None, segEnd).map { case (d, _) => segEnd -> d }
          else Seq.empty

        nextSegment +: findPathSegments(segmentsLeft.tail ++ newToDo)
      }
    }

    @tailrec
    def walkToSplit(currentPath: Seq[Coordinate], prevDir: Direction): SegmentAndLength = {
      val currentLoc = currentPath.last      
      val atJunction = 1 < grid.getCardinalNeighbouringPoints(currentLoc)
        .map(grid.get).count(arrows.values.toSeq.contains)
      if currentLoc == target || atJunction
      then (currentPath.head, currentPath.last) -> (currentPath.length - 1)
      else {
        val possibleMoves = getNextDirs(grid, Some(prevDir), currentLoc)
        val (d, c) = possibleMoves.head
        walkToSplit(currentPath :+ c, d)
      }
    }

    findPathSegments(Seq(start -> Down)).toMap
  }

  private def getNextDirs(grid: Grid[Char], prevDir: Option[Direction], currentLoc: Coordinate) = {
    for d <- Seq(Up, Down, Left, Right)
        if !prevDir.contains(d.opposite)
        c = currentLoc.moveOne(d)
        if grid.containsPoint(c)
        v = grid.get(c)
        if v == '.' || v == arrows(d)
    yield (d, c)
  }

  def findAllPaths(start: Coordinate, target: Coordinate, map: Map[Coordinate, Seq[Coordinate]]): Seq[Seq[Coordinate]] = {
    def helper(currPath: Seq[Coordinate]): Seq[Seq[Coordinate]] = {
      if currPath.last == target then Seq(currPath) else {
        val nextNodes = map(currPath.last)
        nextNodes.foldLeft(Seq.empty)((acc, n) => acc ++ helper(currPath :+ n))
      }
    }

    helper(Seq(start))
  }

  def solvePart1(lines: List[String]): Int = {
    val grid = Grid.fromInput(lines)
    val start = Coordinate(1,0)
    val target = Coordinate(grid.width-2, grid.height-1)
    val graph = buildPathGraph(grid, start, target)
    val nodeGraph = graph.keys.toSeq.groupBy(_._1).view.mapValues(v => v.map(_._2)).toMap
    val paths = findAllPaths(start, target, nodeGraph)
    val distances = paths.map(_.sliding(2).map(_.toSeq).map { case Seq(a, b) => graph((a, b)) }.sum)
    distances.max
  }

  def findAllPathsEitherWay(start: Coordinate, target: Coordinate, map: Map[Coordinate, Seq[Coordinate]]): Seq[Seq[Coordinate]] = {
    def helper(currPath: Seq[Coordinate]): Seq[Seq[Coordinate]] = {
      if currPath.last == target then Seq(currPath)
      else {
        val nextNodes = map(currPath.last).filterNot(currPath.contains)
        if nextNodes.isEmpty then Seq.empty // no possible path to end
        else {
          nextNodes.foldLeft(Seq.empty)((acc, n) => {
            acc ++ helper(currPath :+ n)
          })
        }
      }
    }

    helper(Seq(start))
  }

  def solvePart2(lines: List[String]): Int = {
    val grid = Grid.fromInput(lines)
    val start = Coordinate(1, 0)
    val target = Coordinate(grid.width - 2, grid.height - 1)
    val graph = buildPathGraph(grid, start, target)
    val biDirGraph = graph.map { case (r, d) => (r.swap, d) } ++ graph
    val nodeGraph = biDirGraph.keys.toSeq.groupBy(_._1).view.mapValues(v => v.map(_._2)).toMap
    val paths = findAllPathsEitherWay(start, target, nodeGraph)
    val distances = paths.map(_.sliding(2).map(_.toSeq).map { case Seq(a, b) => biDirGraph((a, b)) }.sum)
    distances.max
  }
}
