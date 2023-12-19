package com.rab.aoc

import com.rab.aoc.helpers._
import scala.util.chaining._

object Day18 {
  case class DiggerCommand(direction: Direction, distance: Int)

  private val lineRegex = """([UDLR]) (\d+)""".r
  def parseInput(lines: List[String]): Seq[DiggerCommand] = {
    lines.map(lineRegex.findFirstMatchIn).map(_.get).map(m => {
      val direction = m.group(1) match
        case "U" => Up
        case "D" => Down
        case "L" => Left
        case "R" => Right
        case _ => throw new RuntimeException("Unrecognised direction in input")
      DiggerCommand(direction, m.group(2).toInt)
    })
  }

  private def getVertices(commands: Seq[DiggerCommand]): Seq[LongCoordinate] = {
    commands.foldLeft(Seq(LongCoordinate(0, 0)))((c, cmd) => {
      c :+ c.last.moveN(cmd.distance, cmd.direction)
    })
  }

  private def getPolygonArea(vertices: Seq[LongCoordinate]): Long = {
    val xx = vertices.map(_.x)
    val yy = vertices.map(_.y)
    val overlace = xx zip yy.drop(1)++yy.take(1)
    val underlace = yy zip xx.drop(1)++xx.take(1)

    (overlace.map(t => t._1 * t._2).sum - underlace.map(t => t._1 * t._2).sum).abs / 2
  }

  private def getLagoonSize(vertices: Seq[LongCoordinate]): Long = {
    val perimeter = vertices.sliding(2).map { case Seq(a, b) => a.manhattanDistance(b) }.sum
    val area = getPolygonArea(vertices)
    area + perimeter/2 + 1
  }

  def solvePart1(lines: List[String]): Long = {
    val commands = parseInput(lines)
    val coordinates = getVertices(commands)
    getLagoonSize(coordinates)
  }

  val hexLineRegex = """#([\dabcdef]{6})""".r
  def parseHexInput(lines: List[String]): Seq[DiggerCommand] = {
    lines.map(hexLineRegex.findFirstMatchIn).map(_.get).map(m => {
      val hexCode = m.group(1)
      val distance = Integer.parseInt(hexCode.take(5), 16)
      val direction = hexCode(5) match
        case '0' => Right
        case '1' => Down
        case '2' => Left
        case '3' => Up
        case _ => throw new RuntimeException("Unparsable command")

      DiggerCommand(direction, distance)
    })
  }

  def solvePart2(lines: List[String]): Long = {
    val commands = parseHexInput(lines)
    val coordinates = getVertices(commands)
    getLagoonSize(coordinates)
  }
}
