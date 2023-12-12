package com.rab.aoc

object Day12 {
  case class SpringRow(springs: Seq[Char], brokenGroupLengths: Seq[Int])

  object SpringRow {
    def valid(sr: SpringRow): Boolean = {
      case class State(brokenLengths: Seq[Int], openRun: Boolean)
      val result = sr.springs.foldLeft(State(Seq.empty, false))((acc, s) => {
        val State(b, open) = acc
        s match {
          case '.' => acc.copy(openRun = false)
          case '#' if open => acc.copy(brokenLengths = b.updated(b.length-1, b.last+1))
          case '#' => State(b :+ 1, true)
          case _ => throw new RuntimeException("Cannot calculate validity with unknowns")
        }
      })
      result.brokenLengths == sr.brokenGroupLengths
    }

    def generatePossibilities(sr: SpringRow): Seq[SpringRow] = {
      val unknownIndex = sr.springs.indexOf('?')
      val someSprings = if unknownIndex < 0 then Seq(sr) else {
        generatePossibilities(sr.copy(springs = sr.springs.updated(unknownIndex, '.'))) ++
        generatePossibilities(sr.copy(springs = sr.springs.updated(unknownIndex, '#')))
      }
      someSprings.filter(valid)
    }

    def unfold(sr: SpringRow): SpringRow = {
      val unfoldedSprings = Seq.fill(5)(sr.springs.mkString).mkString("?")
      val newBrokenengths =  Seq.fill(5)(sr.brokenGroupLengths).flatten
      SpringRow(unfoldedSprings.toCharArray, newBrokenengths)
    }
  }

  private val lineRegex = "([?.#]+) ([\\d+,*]+)".r
  def parseInputLine(s: String): SpringRow = {
    val m = lineRegex.findFirstMatchIn(s).get
    SpringRow(m.group(1), m.group(2).split(',').map(_.toInt))
  }

  def solvePart1(lines: List[String]): Int = {
    lines.map(parseInputLine)
      .map(SpringRow.generatePossibilities)
      .map(_.length)
      .sum
  }

  def solvePart2(lines: List[String]): Int = {
    lines.map(parseInputLine)
      .map(SpringRow.unfold)
      .map(SpringRow.generatePossibilities)
      .map(_.length)
      .sum
  }
}
