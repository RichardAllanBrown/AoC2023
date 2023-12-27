package com.rab.aoc

object Day24 {
  case class Vector3(x: Double, y: Double, z: Double)
  case class Hailstone(pos: Vector3, vel: Vector3)

  def intersectsXY(a: Hailstone, b: Hailstone): Option[(Double, Double)] = {
    val av = a.vel.y / a.vel.x
    val bv = b.vel.y / b.vel.x

    val xIntercept = (av*a.pos.x - bv*b.pos.x + b.pos.y - a.pos.y) / (av - bv)
    val yIntercept = av * (xIntercept-a.pos.x) + a.pos.y

    val timeA = (xIntercept - a.pos.x) / a.vel.x
    val timeB = (xIntercept - b.pos.x) / b.vel.x

    if timeA <= 0 || timeB <= 0 then None else Some((xIntercept, yIntercept))
  }

  def parse(input: List[String]): Seq[Hailstone] = {
    val lineRegex = """(\d+), (\d+), (\d+) @ (-?\d+), (-?\d+), (-?\d+)""".r
    input.map(lineRegex.findFirstMatchIn).map(_.get).map(m => {
      def getVal(i: Int) = m.group(i).toLong
      val position = Vector3(getVal(1), getVal(2), getVal(3))
      val velocity = Vector3(getVal(4), getVal(5), getVal(6))
      Hailstone(position, velocity)
    })
  }

  private val minRange = 200_000_000_000_000D
  private val maxRange = 400_000_000_000_000D
  def isInArea(x: Double, y: Double): Boolean = {
    minRange < x && x < maxRange &&
      minRange < y && y < maxRange
  }

  def solvePart1(input: List[String]): Long = {
    val hailstones = parse(input)
    val collisions = hailstones.combinations(2)
      .flatMap { case Seq(a, b) => intersectsXY(a, b) }
      .filter((x, y) => isInArea(x, y))
    collisions.length
  }
}
