package com.rab.aoc

import com.rab.aoc.Day16._

class Day16Test extends UnitSpec {
  val exampleInput = List(
    """.|...\....""",
    """|.-.\.....""",
    """.....|-...""",
    """........|.""",
    """..........""",
    """.........\""",
    """..../.\\..""",
    """.-.-/..|..""",
    """.|....-|.\""",
    """..//.|...."""
  )

  test("Works for example") {
    solvePart1(exampleInput) shouldEqual 46
  }
}
