package com.rab.aoc.helpers

import scala.reflect.ClassTag

case class Grid[T](width: Int, height: Int, values: Seq[T])(implicit gct: ClassTag[T]) {
  assert(values.length == width*height, "The height and width does not match values given")
  private def toIndex = Grid.toIndex(width)
  private def toCoord = Grid.toCoord(width)

  def get(c: Coordinate): T = values(toIndex(c))

  def allPoints: Seq[Coordinate] = {
    for x <- 0 until width
        y <- 0 until height
    yield Coordinate(x, y)
  }

  def containsPoint(c: Coordinate): Boolean = {
    0 <= c.x && c.x < width && 0 <= c.y && c.y < height
  }

  def getCardinalNeighbouringPoints(c: Coordinate): Seq[Coordinate] = {
    c.cardinalNeighbours.filter(containsPoint)
  }

  def findFirstPoint(f: T => Boolean): Option[Coordinate] = {
    val i = values.indexWhere(f)
    if i < 0 then None else Some(toCoord(i))
  }

  def findPoints(f: T => Boolean): Seq[Coordinate] = {
    values.zipWithIndex.collect { case (v, i) if f(v) => i }.map(toCoord)
  }

  def map[A](f: T => A)(implicit ct: ClassTag[A]): Grid[A] = {
    copy(values = values.map(f))
  }

  def setValue(c: Coordinate, v: T): Grid[T] = {
    copy(values = values.updated(toIndex(c), v))
  }

  def rotateClockwise: Grid[T] = {
    val rows = values.sliding(width, width).toSeq
    val newValues = (0 until width).reverse.flatMap(w => rows.map(_(w))).reverse
    Grid(height, width, newValues)
  }

  override def toString: String = {
    values.sliding(width, width).mkString("\n")
  }
}

object Grid {
  def fill[T](width: Int, height: Int)(f: => T)(implicit ct: ClassTag[T]): Grid[T] = {
    Grid(width, height, Array.fill(width * height)(f))
  }
  def fromInput(input: List[String]): Grid[Char] = {
    val width = input.head.length
    val height = input.length
    Grid(width, height, input.mkString.toCharArray)
  }
  def toIndex(width: Int)(c: Coordinate): Int = c.y * width + c.x
  def toCoord(width: Int)(i: Int): Coordinate = Coordinate(i % width, i / width)
  def fromCoordinates(c: Set[Coordinate]): Grid[Boolean] = {
    val width = c.map(_.x).max + 1
    val height = c.map(_.y).max + 1
    val values = (0 until height*width).map(i => c.contains(toCoord(width)(i))).toArray
    Grid(width, height, values)
  }
}
