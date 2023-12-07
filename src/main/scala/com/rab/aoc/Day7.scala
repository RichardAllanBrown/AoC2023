package com.rab.aoc

object Day7 {
  case class Card(value: Int) {
    override def toString: String = {
      value match
        case 14 => "A"
        case 13 => "K"
        case 12 => "Q"
        case 11 => "J"
        case 10 => "T"
        case 1 => "*"
        case _ => value.toString
    }
  }
  object Card {
    def fromChar(c: Char, jokers: Boolean = false): Card = c match
      case 'A' => Card(14)
      case 'K' => Card(13)
      case 'Q' => Card(12)
      case 'J' if !jokers => Card(11)
      case 'J' if jokers => Card(1)
      case 'T' => Card(10)
      case a if a.isDigit => Card(a.asDigit)
      case _ => throw new RuntimeException(s"Cannot parse char '$c' to a known card'")
    val joker: Card = Card(1)
    val allNonJokerCards: Seq[Card] = (2 to 14).map(Card.apply)
  }

  enum HandType:
    case HighCard
    case OnePair
    case TwoPair
    case ThreeOfAKind
    case FullHouse
    case FourOfAKind
    case FiveOfAKind

  case class Hand(cards: Seq[Card], bid: Int) {
    import HandType._
    assert(cards.length == 5, "Hand must have 5 cards")

    private val counts = cards
      .filter(_ != Card.joker)
      .groupBy(_.value)
      .values
      .map(_.length)
      .toSeq

    val handType: HandType = {
      val jokerCount = cards.count(_ == Card.joker)
      if jokerCount == 5 || jokerCount == 4 then FiveOfAKind
      else if jokerCount == 3 && counts.contains(2) then FiveOfAKind
      else if jokerCount == 3 then FourOfAKind
      else if jokerCount == 2 && counts.contains(3) then FiveOfAKind
      else if jokerCount == 2 && counts.contains(2) then FourOfAKind
      else if jokerCount == 2 then ThreeOfAKind
      else if jokerCount == 1 && counts.contains(4) then FiveOfAKind
      else if jokerCount == 1 && counts.contains(3) then FourOfAKind
      else if jokerCount == 1 && counts.count(_ == 2) == 2 then FullHouse
      else if jokerCount == 1 && counts.contains(2) then ThreeOfAKind
      else if jokerCount == 1 then OnePair
      else if counts.contains(5) then FiveOfAKind
      else if counts.contains(4) then FourOfAKind
      else if counts.contains(3) && counts.contains(2) then FullHouse
      else if counts.contains(3) then ThreeOfAKind
      else if counts.count(_ == 2) == 2 then TwoPair
      else if counts.contains(2) then OnePair
      else HighCard
    }
  }
  implicit object HandOrdering extends Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      if x.handType != y.handType then x.handType.ordinal.compare(y.handType.ordinal)
      else (0 to 4).collectFirst {
        case i if x.cards(i) != y.cards(i) => x.cards(i).value.compare(y.cards(i).value)
      }.getOrElse(0)
    }
  }

  def parseHand(s: String, jokers: Boolean = false): Hand = {
    val cards = s.take(5).map(c => Card.fromChar(c, jokers))
    val bid = s.drop(6).toInt
    Hand(cards, bid)
  }

  def scoreHands(lines: List[String], jokers: Boolean): Long = {
    val sorted = lines.map(l => parseHand(l, jokers)).sorted
    sorted.zipWithIndex.map {
      case (hand, i) => (hand.bid * (i + 1)).toLong
    }.sum
  }

  def solvePart1(lines: List[String]): Long = scoreHands(lines, false)

  def solvePart2(lines: List[String]): Long = scoreHands(lines, true)
}
