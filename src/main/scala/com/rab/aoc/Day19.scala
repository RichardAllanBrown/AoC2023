package com.rab.aoc

import scala.annotation.tailrec

object Day19 {
  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def getPropValue(c: Char) = c match
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s
      case _ => throw new IllegalArgumentException(s"Unrecognised prop char '$c'")

    lazy val totalRating: Int = x + m + a + s
  }

  sealed trait Outcome
  case object Accept extends Outcome
  case object Reject extends Outcome
  case class GoToRule(ruleKey: String) extends Outcome

  case class RulePart(partProp: Char, comparator: Char, value: Int, goto: Outcome)
  case class Rule(parts: Seq[RulePart], otherwise: Outcome) {
    def evaluate(part: Part): Outcome = {
      parts.collectFirst({
        case RulePart(partProp, '<', value, goto) if part.getPropValue(partProp) < value => goto
        case RulePart(partProp, '>', value, goto) if part.getPropValue(partProp) > value => goto
      }).getOrElse(otherwise)
    }
  }

  val ruleParser = """^(\w+)\{([\w<>:,]+)}""".r
  val partParser = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}""".r
  def parse(lines: List[String]): (Map[String, Rule], Seq[Part]) = {
    val splitAt = lines.indexWhere(l => l.isEmpty)
    val rules = lines.take(splitAt).map(ruleParser.findFirstMatchIn).map(_.get).map { m =>
      m.group(1) -> parseRule(m.group(2))
    }
    val parts = lines.drop(splitAt+1).map(partParser.findFirstMatchIn).map(_.get).map { m =>
      Part(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt)
    }
    (rules.toMap, parts)
  }

  val rulePartRegex = """([xmas])([<>])(\d+):(\w+)""".r
  def parseRule(rule: String): Rule = {
    val splitRule =  rule.split(',')
    val ruleParts = splitRule.flatMap(phrase => {
      rulePartRegex.findFirstMatchIn(phrase).map(m => {
        RulePart(m.group(1).head, m.group(2).head, m.group(3).toInt, parseOutcome(m.group(4)))
      })
    })
    val default = parseOutcome(splitRule.last)
    Rule(ruleParts, default)
  }

  private def parseOutcome(str: String): Outcome = {
    str match
      case "A" => Accept
      case "R" => Reject
      case s => GoToRule(s)
  }

  def isAccepted(ruleMap: Map[String, Rule])(part: Part): Boolean = {
    @tailrec
    def helper(ruleKey: String): Boolean = {
      ruleMap(ruleKey).evaluate(part) match
        case Accept => true
        case Reject => false
        case GoToRule(ruleKey) => helper(ruleKey)
    }
    helper("in")
  }

  def solvePart1(lines: List[String]): Int = {
    val (ruleMap, parts) = parse(lines)
    parts.filter(isAccepted(ruleMap)).map(_.totalRating).sum
  }
}
