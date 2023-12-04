package day02

import scala.util.parsing.combinator.RegexParsers
import aoc.ParseException

@main
def main(filepath: String): Unit =

  val input  = aoc.readInput(filepath)
  val parser = new GameParser
  val games  = input.map(parser.parseGame)

  val score1 = games.filter(game => possible(game, 12, 13, 14)).map(_.id).sum
  val score2 = games.map { game =>
    game.reveals.map(_.red).max *
      game.reveals.map(_.green).max *
      game.reveals.map(_.blue).max
  }.sum

  println(s"${"=" * 8} Day 02 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def possible(game: Game, maxRed: Int, maxGreen: Int, maxBlue: Int): Boolean =
  game.reveals.forall(r => r.red <= maxRed && r.green <= maxGreen && r.blue <= maxBlue)

case class Reveal(red: Int, green: Int, blue: Int)
case class Game(id: Int, reveals: List[Reveal])

class GameParser extends RegexParsers:

  def parseGame(line: String): Game = parse(game, line) match
    case Success(result, next) if next.atEnd => result
    case _                                   => throw new ParseException(line, "game")

  def game: Parser[Game] = "Game" ~> integer ~ ":" ~ reveals ^^ { case (id ~ _ ~ reveals) =>
    Game(id, reveals)
  }

  def integer: Parser[Int] = """(\d+)""".r ^^ { value => value.toInt }

  def reveals: Parser[List[Reveal]] = repsep(reveal, ";")

  def reveal: Parser[Reveal] = repsep(balls, ",") ^^ {
    _.foldLeft(Reveal(0, 0, 0)) { case (r, (n, c)) =>
      c match {
        case "red"   => r.copy(red = r.red + n)
        case "green" => r.copy(green = r.green + n)
        case "blue"  => r.copy(blue = r.blue + n)
      }
    }
  }

  def balls: Parser[(Int, String)] = integer ~ color ^^ { case n ~ c => (n, c) }

  def color: Parser[String] = "red" | "blue" | "green"
