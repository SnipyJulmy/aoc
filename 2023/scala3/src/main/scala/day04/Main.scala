package day04

import scala.util.parsing.combinator.RegexParsers
import aoc.ParseException
import scala.collection.mutable.PriorityQueue

@main
def main(filepath: String): Unit =

  val input  = aoc.readInput(filepath)
  val parser = new CardParser
  val cards  = input.map(parser.parseCard)

  val score1 = cards.map(c => pow2(c.matchingNumbers - 1)).sum
  val score2 = countScratchCards(cards)

  println(s"${"=" * 8} Day 04 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def countScratchCards(cards: List[Card]): Int =
  val cardMap: Map[Int, Card] = cards.map(c => c.id -> c).toMap
  def inner(cards: PriorityQueue[Int], acc: Int): Int =
    if cards.isEmpty then acc
    else
      val id   = cards.dequeue()
      val card = cardMap(id)
      for (i <- id + 1 to id + card.matchingNumbers) {
        cards.addOne(i)
      }
      inner(cards, acc + 1)
  inner(PriorityQueue.from(cards.map(_.id)), 0)

def pow2(n: Int): Int = n match
  case n if n < 0 => 0
  case 0          => 1
  case n          => 2 * pow2(n - 1)

case class Card(id: Int, winningNumbers: Set[Int], obtainedNumbers: Set[Int]):
  lazy val matchingNumbers: Int = (winningNumbers intersect obtainedNumbers).size

class CardParser extends RegexParsers:

  def parseCard(line: String): Card = parse(card, line) match
    case Success(result, next) if next.atEnd => result
    case _                                   => throw new ParseException(line, "card")

  def card: Parser[Card] = "Card" ~> (integer ~ (":" ~> integers ~ ("|" ~> integers))) ^^ { case id ~ (winnings ~ obtained) =>
    Card(id, winnings.toSet, obtained.toSet)
  }

  def integers: Parser[List[Int]] = rep(integer)

  def integer: Parser[Int] = """(\d+)""".r ^^ { value => value.toInt }
