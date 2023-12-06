package com.rab.aoc

import scala.annotation.targetName

object Day5 {
  case class Range(start: Long, end: Long) {
    assert(start <= end, "The end must be equal to or greater than the start")
    def intersectsWith(other: Range): Boolean = {
      (start <= other.start && other.start <= end) || (start <= other.end && other.end <= end)
    }
    def totallyContains(other: Range): Boolean = {
      start <= other.start && other.end <= end
    }
    def length: Long = end - start
    @targetName("add")
    def +(value: Long): Range = Range(start+value, end+value)
  }
  object Range {
    def ofOne(value: Long): Range = Range(value, value)
  }

  case class RangedAddition(range: Range, valueToAdd: Long) {
    // returns the modified input if possible and remaining, untouched ranges
    def applyTo(input: Range): (Option[Range], Seq[Range]) = {
      if (range.totallyContains(input)) {
        // this range covers the entire input, no remainder
        (Some(input + valueToAdd), Seq.empty)
      } else if (!range.intersectsWith(input)) {
        // the ranges do not intersect, so the input is unaffected
        (None, Seq(input))
      } else {
        val preChunk = if input.start < range.start then Some(Range(input.start, range.start-1)) else None
        val midChunk = Range(math.max(range.start, input.start), math.min(range.end, input.end))
        val postChunk = if range.end < input.end then Some(Range(range.end+1, input.end)) else None
        (Some(midChunk+valueToAdd), Seq(preChunk, postChunk).flatten)
      }
    }
  }

  case class MultiRangeAddition(ranges: Seq[RangedAddition]) {
    def applyTo(input: Range): Seq[Range] = {
      val subInputRanges = ranges.foldLeft((Seq[Range](), Seq(input)))((acc, r) => {
        val (processed, remaining) = acc
        val result = remaining.map(r.applyTo)
        val newlyProcessed = result.flatMap(_._1)
        (processed ++ newlyProcessed, result.flatMap(_._2))
      })
      subInputRanges._1 ++ subInputRanges._2
    }
  }

  private def parseRangeAdditionFromInput(s: String): RangedAddition = {
    s.split(' ').map(_.toLong) match
      case Array(destStart, sourceStart, length) =>
        val addition = destStart - sourceStart
        RangedAddition(Range(sourceStart, sourceStart + length), addition)
      case _ => throw new RuntimeException(s"Cannot parse range line $s")
  }

  private def getAlmanacMap(lines: List[String])(header: String): MultiRangeAddition = {
    val mapHeaderLineIndex = lines.indexOf(header)
    val mappingLines = lines.drop(mapHeaderLineIndex + 1).takeWhile(!_.isBlank)
    MultiRangeAddition(mappingLines.map(parseRangeAdditionFromInput))
  }

  def solveDay5Part1(lines: List[String]): Long = {
    val seeds = lines.head.drop(6).split(' ').filter(!_.isBlank).map(_.toLong).map(Range.ofOne).toSeq
    val mapGen = getAlmanacMap(lines)

    val mapSeq = Seq(
      mapGen("seed-to-soil map:"),
      mapGen("soil-to-fertilizer map:"),
      mapGen("fertilizer-to-water map:"),
      mapGen("water-to-light map:"),
      mapGen("light-to-temperature map:"),
      mapGen("temperature-to-humidity map:"),
      mapGen("humidity-to-location map:")
    )

    mapSeq.foldLeft(seeds)((s, m) => {
      s.flatMap(m.applyTo)
    }).map(_.start).min
  }

  def solveDay5Part2(lines: List[String]): Long = {
    val seeds = lines.head.drop(6).split(' ').filter(!_.isBlank).map(_.toLong).grouped(2)
      .collect { case Array(start, length) => Range(start, start+length) }.toSeq

    val mapGen = getAlmanacMap(lines)
    val mapSeq = Seq(
      mapGen("seed-to-soil map:"),
      mapGen("soil-to-fertilizer map:"),
      mapGen("fertilizer-to-water map:"),
      mapGen("water-to-light map:"),
      mapGen("light-to-temperature map:"),
      mapGen("temperature-to-humidity map:"),
      mapGen("humidity-to-location map:")
    )

    mapSeq.foldLeft(seeds)((s, m) => {
      s.flatMap(m.applyTo)
    }).map(_.start).min
  }
}
