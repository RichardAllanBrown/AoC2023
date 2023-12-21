package com.rab.aoc.helpers

import scala.annotation.targetName

case class Range(start: Long, end: Long) {
  assert(start <= end, "The end must be equal to or greater than the start")

  def intersectsWith(other: Range): Boolean = {
    (start <= other.start && other.start <= end) ||
      (start <= other.end && other.end <= end) ||
      (other.start <= start && end <= other.end)
  }

  def totallyContains(other: Range): Boolean = {
    start <= other.start && other.end <= end
  }

  def length: Long = end - start

  @targetName("add")
  def +(value: Long): Range = Range(start + value, end + value)
  
  def splitRangeLessThan(n: Long): (Option[Range], Option[Range]) = {
    if start < n && n <= end then (Some(Range(start, n-1)), Some(Range(n, end)))
    else if n <= start then (None, Some(this))
    else (Some(this), None)
  }
  
  def splitRangeMoreThan(n: Long): (Option[Range], Option[Range]) = {
    if start <= n && n < end then (Some(Range(start, n)), Some(Range(n+1, end)))
    else if n < start then (None, Some(this))
    else (Some(this), None)
  }
  
  def sum: Long = (end - start+ 1) * (start + end) / 2
}

object Range {
  def ofOne(value: Long): Range = Range(value, value)
}
