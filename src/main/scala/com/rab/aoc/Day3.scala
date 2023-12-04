package com.rab.aoc

import scala.annotation.tailrec

object Day3 {
  case class Coord(x: Int, y: Int)

  sealed trait Elem
  case class PartNumber(start: Coord, value: Int) extends Elem {
    def adjacentTo(s: Symbol): Boolean = {
      val Symbol(Coord(x, y), _) = s
      val lowerY = start.y - 1
      val upperY = start.y + 1
      val lowerX = start.x - 1
      val upperX = start.x + value.toString.length
      lowerX <= x && x <= upperX && lowerY <= y && y <= upperY
    }
  }
  case class Symbol(loc: Coord, value: Char) extends Elem

  case class Schematic(elements: List[Elem]) {
    lazy val symbols: Seq[Symbol] = elements.collect { case s: Symbol => s }
    lazy val parts: Seq[PartNumber] = elements.collect { case p: PartNumber => p }

    def getTruePartNumbers: Seq[PartNumber] = {
      parts.filter(p => symbols.exists(s => p.adjacentTo(s)))
    }
  }

  def parseSchematic(input: List[String]): Schematic = {
    val elements = input.zipWithIndex.flatten((line, y) => {
      @tailrec
      def lineParseHelper(currIndex: Int, currItems: List[Elem]): List[Elem] = {
        assert(currIndex >= 0, "A negative index was passed!")
        if line.length <= currIndex then currItems
        else {
          val currentVal = line(currIndex)
          if currentVal == '.' then
            lineParseHelper(currIndex + 1, currItems)
          else if !currentVal.isDigit then
            val elem = Symbol(Coord(currIndex, y), currentVal)
            lineParseHelper(currIndex + 1, currItems :+ elem)
          else
            val nextNonDigitIndex = line.indexWhere(!_.isDigit, currIndex)
            val endIndex = if nextNonDigitIndex == -1 then line.length else nextNonDigitIndex
            val number = line.substring(currIndex, endIndex).toInt
            val elem = PartNumber(Coord(currIndex, y), number)
            lineParseHelper(endIndex, currItems :+ elem)
        }
      }

      lineParseHelper(0, Nil)
    })

    Schematic(elements)
  }

  def solveDay3Part1(input: List[String]): Int = {
    val schematic = parseSchematic(input)
    val truePartNumbers = schematic.getTruePartNumbers
    truePartNumbers.map(_.value).sum
  }

  def solveDay3Part2(input: List[String]): Int = {
    val schematic = parseSchematic(input)
    schematic.symbols
      .filter(_.value == '*')
      .map(g => schematic.parts.filter(_.adjacentTo(g)))
      .collect { case Seq(first, second) => first.value * second.value }
      .sum
  }
}
