package com.rab.aoc

object Day9 {
  def solveLayer(values: Seq[Int]): Seq[Int] = {
    values.sliding(2).map(_.toSeq).map({ case Seq(a: Int, b: Int) => b - a }).toSeq
  }

  def predictNextOasisNo(readings: Seq[Int]): Int = {
    if readings.forall(_ == 0) then 0
    else {
      val nextLayer = solveLayer(readings)
      val nextNo = predictNextOasisNo(nextLayer)
      readings.last + nextNo
    }
  }

  def predictPrevOasisNo(readings: Seq[Int]): Int = {
    if readings.forall(_ == 0) then 0
    else {
      val nextLayer = solveLayer(readings)
      val prevNo = predictPrevOasisNo(nextLayer)
      readings.head - prevNo
    }
  }

  def solvePart1(lines: Seq[String]): Int = {
    lines.map(_.split(' ').map(_.toInt).toSeq)
      .map(predictNextOasisNo)
      .sum
  }

  def solvePart2(lines: Seq[String]): Int = {
    lines.map(_.split(' ').map(_.toInt).toSeq)
      .map(predictPrevOasisNo)
      .sum
  }
}
