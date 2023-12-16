package com.rab.aoc.helpers

sealed trait Direction {
  val opposite: Direction
}
case object Up extends Direction {
  override val opposite: Direction = Down
}
case object Down extends Direction {
  override val opposite: Direction = Up
}
case object Left extends Direction {
  override val opposite: Direction = Right
}
case object Right extends Direction {
  override val opposite: Direction = Left
}
