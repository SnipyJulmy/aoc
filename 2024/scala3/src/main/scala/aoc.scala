package aoc

import scala.util.Using
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

def readInput(filepath: String) =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse {
    println("Unable to read $filepath")
    sys.exit(1)
  }

def readInputAsStream(filepath: String) =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.toList.toStream
  }.getOrElse {
    println("Unable to read $filepath")
    sys.exit(1)
  }

def readInputAsMatrix[T <: Any](filepath: String, transform: Char => T = identity): Vector[Vector[T]] =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().map(_.map(transform).toVector).toVector
  }.getOrElse {
    println("Unable to read $filepath")
    sys.exit(1)
  }

class ParseException(input: String, parser: String = "noname") extends Exception

trait Problem

abstract class ProblemParser[A <: Problem] extends RegexParsers:

  def parse(lines: List[String]): A =
    val input = lines.mkString("\n")
    parse(problem, input) match
      case Success(result, next) if next.atEnd => result
      case _                                   => throw new ParseException(input, "problem")

  def problem: Parser[A]

  def integers: Parser[List[Int]] = rep(integer)

  def integer: Parser[Int] = """(\d+)""".r ^^ { value => value.toInt }
