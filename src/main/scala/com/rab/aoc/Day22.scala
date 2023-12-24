package com.rab.aoc

import com.rab.aoc.helpers.*

import scala.annotation.tailrec

object Day22 {
  case class Vec3(x: Int, y: Int, z: Int) {
    def moveZ(d: Int): Vec3 = copy(z = z + d)
  }
  
  case class Brick(a: Vec3, b: Vec3) {
    def minX: Int = math.min(a.x, b.x)
    def maxX: Int = math.max(a.x, b.x)
    def minY: Int = math.min(a.y, b.y)
    def maxY: Int = math.max(a.y, b.y)
    def minZ: Int = math.min(a.z, b.z)
    def maxZ: Int = math.max(a.z, b.z)
    def xRange: Range = Range(minX, maxX)
    def yRange: Range = Range(minY, maxY)
    
    def shiftDown(dist: Int): Brick = {
      Brick(a.moveZ(-dist), b.moveZ(-dist))
    }
    
    def interceptsXY(other: Brick): Boolean = {
      xRange.intersectsWith(other.xRange) && yRange.intersectsWith(other.yRange)
    }

    def touches(other: Brick): Boolean = {
      interceptsXY(other) && (maxZ + 1 == other.minZ || other.maxZ + 1 == minZ)
    }
  }

  def parseInput(lines: List[String]): Seq[Brick] = {
    val lineRegex = """(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)""".r
    lines.map(lineRegex.findFirstMatchIn).map(_.get).map(m => {
      val values = (1 to 6).map(m.group).map(_.toInt)
      Brick(Vec3(values(0), values(1), values(2)), Vec3(values(3), values(4), values(5)))
    })
  }

  def settle(bricks: Seq[Brick]): Seq[Brick] = {
    @tailrec
    def helper(bricksLeft: Seq[Brick], settled: Seq[Brick]): Seq[Brick] = {
      if bricksLeft.isEmpty then settled else {
        val b = bricksLeft.head
        val settledMaxZ = settled.filter(_.interceptsXY(b)).map(_.maxZ).maxOption.getOrElse(0)
        val shiftedBrick = b.shiftDown(b.minZ - settledMaxZ - 1)
        helper(bricksLeft.tail, settled :+ shiftedBrick)
      }
    }
    helper(bricks.sortBy(_.minZ), Seq.empty)
  }

  def findSafelyDisintegratable(bricks: Seq[Brick]): Seq[Brick] = {
    val maxX = bricks.map(_.maxX).max
    val maxY = bricks.map(_.maxY).max
    val ground = Brick(Vec3(0,0,0), Vec3(maxX, maxY, 0))

    val byMaxZ = (bricks :+ ground).groupBy(_.maxZ)
    val brickAndWhatItSitsOn = bricks.map(b => {
      b -> byMaxZ.getOrElse(b.minZ-1, Seq.empty).filter(_.interceptsXY(b))
    }).toMap
    val byMinZ = bricks.groupBy(_.minZ)
    val brickAndWhatSitsOnIt = bricks.map(b => {
      b -> byMinZ.getOrElse(b.maxZ+1, Seq.empty).filter(_.interceptsXY(b))
    }).toMap

    brickAndWhatSitsOnIt.filter((b, bOnIt) => {
      bOnIt.isEmpty || bOnIt.forall(bon => !brickAndWhatItSitsOn(bon).forall(_ == b))
    }).keys.toSeq
  }

  def solvePart1(lines: List[String]): Int = {
    val bricks = parseInput(lines)
    val settledBricks = settle(bricks)
    val freeBricks = findSafelyDisintegratable(settledBricks)
    freeBricks.length
  }

  def countBricksThatMove(bricks: Seq[Brick]): Int = {
    val maxX = bricks.map(_.maxX).max
    val maxY = bricks.map(_.maxY).max
    val ground = Brick(Vec3(0, 0, 0), Vec3(maxX, maxY, 0))

    val byMaxZ = (bricks :+ ground).groupBy(_.maxZ)
    val brickAndWhatItSitsOn = bricks.map(b => {
      b -> byMaxZ.getOrElse(b.minZ - 1, Seq.empty).filter(_.interceptsXY(b))
    }).toMap
    val byMinZ = bricks.groupBy(_.minZ)
    val brickAndWhatSitsOnIt = bricks.map(b => {
      b -> byMinZ.getOrElse(b.maxZ + 1, Seq.empty).filter(_.interceptsXY(b))
    }).toMap

    def canFindGround(b: Brick, pile: Map[Brick, Seq[Brick]]): Boolean = {
      if pile.contains(b)
      then pile(b).exists(bb => bb == ground || canFindGround(bb, pile))
      else false
    }

    bricks.map { bToRemove =>
      val newBrickStack = brickAndWhatItSitsOn.removed(bToRemove)
      bricks.filterNot(_ == bToRemove).filterNot(canFindGround(_, newBrickStack)).length
    }.sum
  }

  def solvePart2(lines: List[String]): Int = {
    val bricks = parseInput(lines)
    val settledBricks = settle(bricks)
    countBricksThatMove(settledBricks)
  }
}
