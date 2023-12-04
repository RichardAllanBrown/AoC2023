package com.rab.aoc

import com.rab.aoc.Day3._

class Day3Test extends UnitSpec {
  test("Should be able to test adjacency for single digits") {
    val part = PartNumber(Coord(2, 2), 1)
    part.adjacentTo(Symbol(Coord(2, 1), '/')) shouldEqual true
    part.adjacentTo(Symbol(Coord(2, 3), '/')) shouldEqual true
    part.adjacentTo(Symbol(Coord(1, 2), '/')) shouldEqual true
    part.adjacentTo(Symbol(Coord(3, 2), '/')) shouldEqual true
    PartNumber(Coord(2, 2), 1).adjacentTo(Symbol(Coord(4, 4), '/')) shouldEqual false
  }

  private val input = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )

  test("Should parse example") {
    val schematic = parseSchematic(input)
    schematic.parts.length shouldEqual 10
    schematic.symbols.length shouldEqual 6
    schematic.getTruePartNumbers.length shouldEqual 8
  }

  test("Can solve the example") {
    solveDay3Part1(input) shouldEqual 4361
  }
}
