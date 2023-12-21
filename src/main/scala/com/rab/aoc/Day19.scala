package com.rab.aoc

import scala.annotation.tailrec
import com.rab.aoc.helpers.Range

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

  case class RangedPart(x: Range, m: Range, a: Range, s: Range) {
    lazy val totalRating: Long = x.sum * m.sum * a.sum * s.sum
  }
  
  // The left is the part that didn't match and the right is right
  def splitRange(comparator: Char, prop: Char, splitAt: Long)(part: RangedPart): (Option[RangedPart], Option[RangedPart]) = {
    def helper(s: RangedPart => Range, builder: Range => RangedPart) = {
      val (r1, r2) = if comparator == '<' then s(part).splitRangeLessThan(splitAt) else s(part).splitRangeMoreThan(splitAt)
      val result = (r1.map(builder), r2.map(builder))
      if comparator == '<' then result.swap else result
    }
    prop match {
      case 'x' => helper(_.x, r => part.copy(x = r))
      case 'm' => helper(_.m, r => part.copy(m = r))
      case 'a' => helper(_.a, r => part.copy(a = r))
      case 's' => helper(_.s, r => part.copy(s = r))
      case _ => throw new IllegalArgumentException(s"Unrecognised prop value $prop")
    }
  }

  // First is the part matched by the rule, second is the part not matched by rule
  def processRule(rules: Map[String, Rule])(ruleKey: String, part: RangedPart): Seq[RangedPart] = {
    case class State(unmatched: Option[RangedPart], matched: Map[Outcome, Seq[RangedPart]]) {
      def withMatchedRange(outcome: Outcome, rangedPart: RangedPart) = {
        val newValue = matched.getOrElse(outcome, Seq.empty) :+ rangedPart
        copy(matched = matched.updated(outcome, newValue))
      }
    }

    val rule = rules(ruleKey)
    val afterPartsState = rule.parts.foldLeft(State(Some(part), Map.empty))((acc, rp) => {
      acc.unmatched.map(needProc => {
        val (newUnmatched, matched) = splitRange(rp.comparator, rp.partProp, rp.value)(needProc)
        val newMatched = matched.map(m => acc.withMatchedRange(rp.goto, m)).getOrElse(acc)
        newMatched.copy(unmatched = newUnmatched)
      }).getOrElse(acc)
    })

    val endState = afterPartsState.unmatched match
      case Some(u) => afterPartsState.withMatchedRange(rule.otherwise, u)
      case None => afterPartsState

    endState.matched.collect {
      case (GoToRule(nextRuleKey), parts) => parts.flatMap(p => processRule(rules)(nextRuleKey, p))
      case (Accept, parts) => parts
    }.flatten.toSeq
  }

  def solvePart2(lines: List[String]): Long = {
    val (ruleMap, _) = parse(lines)
    val rangedPart = RangedPart(Range(1, 4000), Range(1, 4000), Range(1, 4000), Range(1, 4000))
    processRule(ruleMap)("in", rangedPart).map(_.totalRating).sum
  }
}
