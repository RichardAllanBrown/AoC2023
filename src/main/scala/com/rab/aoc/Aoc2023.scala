package com.rab.aoc

import scala.io.Source
import scala.util.Using

private def getPuzzleInput(day: String): List[String] = {
  Using(Source.fromResource(s"Day${day}Input.txt")) { file =>
    file.getLines().toList
  }.get
}

type Solver = List[String] => Any
private val puzzleDict = Map[Int, (Option[Solver], Option[Solver])](
  1 -> (Some(Day1.solvePart1Puzzle), Some(Day1.solvePart2Puzzle)),
  2 -> (Some(Day2.solvePart1Puzzle), Some(Day2.solvePart2Puzzle)),
  3 -> (Some(Day3.solveDay3Part1), Some(Day3.solveDay3Part2)),
  4 -> (Some(Day4.solveDay4Part1), Some(Day4.solveDay4Part2)),
  5 -> (Some(Day5.solveDay5Part1), Some(Day5.solveDay5Part2)),
  6 -> (Some(Day6.solvePart1), Some(Day6.solvePart2)),
  7 -> (Some(Day7.solvePart1), Some(Day7.solvePart2)),
  8 -> (Some(Day8.solvePart1), Some(Day8.solvePart2)),
  9 -> (Some(Day9.solvePart1), Some(Day9.solvePart2)),
  10 -> (Some(Day10.solvePart1), None),
  11 -> (Some(Day11.solvePart1), Some(Day11.solvePart2)),
  12 -> (Some(Day12.solvePart1), Some(Day12.solvePart2)),
  13 -> (Some(Day13.solvePart1), Some(Day13.solvePart2)),
  14 -> (Some(Day14.solvePart1), Some(Day14.solvePart2)),
  15 -> (Some(Day15.solvePart1), None),
  16 -> (Some(Day16.solvePart1), Some(Day16.solvePart2)),
  17 -> (Some(Day17.solvePart1), Some(Day17.solvePart2)),
  18 -> (Some(Day18.solvePart1), Some(Day18.solvePart2)),
  19 -> (Some(Day19.solvePart1), Some(Day19.solvePart2)),
  20 -> (Some(Day20.solvePart1), None),
  21 -> (Some(Day21.solvePart1), None),
  22 -> (Some(Day22.solvePart1), Some(Day22.solvePart2)),
  23 -> (Some(Day23.solvePart1), Some(Day23.solvePart2)),
  24 -> (Some(Day24.solvePart1), None),
  25 -> (Some(Day25.solvePart1), None)
)

@main def main(args: String*): Unit = {
  println(args.mkString("ReceivedArgs(", ", ", ")"))
  if (args == null || args.length != 1) {
    println("ERR: Please provide a day to run")
    sys.exit(1)
  }

  args.head.toIntOption match
    case None =>
      println(s"ERR: First argument needs to be a number")
      sys.exit(1)
    case Some(selectedDay) if !puzzleDict.contains(selectedDay) =>
      println(s"ERR: No solution available for day $selectedDay")
      sys.exit(1)
    case Some(selectedDay) =>
      val solvers = puzzleDict.get(selectedDay)
      val part1Solver = solvers.flatMap(_._1)
      val part2Solver = solvers.flatMap(_._2)
      val input = getPuzzleInput(args.head)

      part1Solver match
        case Some(solver) =>
          println(s"Computing answer for day $selectedDay, part 1")
          println(s"The solution is: ${solver(input)}")
        case None =>
          println(s"No solver available for day $selectedDay, part 1")

      part2Solver match
        case Some(solver) =>
          println(s"Computing answer for day $selectedDay, part 2")
          println(s"The solution is: ${solver(input)}")
        case None =>
          println(s"No solver available for day $selectedDay, part 2")
}