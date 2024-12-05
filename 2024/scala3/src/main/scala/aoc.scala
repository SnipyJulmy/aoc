package aoc

import scala.util.Using
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.io.BufferedSource
import scala.util.Failure
import scala.util.Success

def readInput(filepath: String) =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  } match
    case Failure(exception) =>
      println(s"Unable to read $filepath : $exception")
      sys.exit(1)
    case Success(value) => value

def readInput[A](filepath: String, parser: ProblemParser[A]): A =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    parser.parse(source)
  } match
    case Failure(exception) =>
      println(s"Unable to read $filepath : $exception")
      sys.exit(1)
    case Success(value) => value

def readInputAsStream(filepath: String) =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.toList.toStream
  } match
    case Failure(exception) =>
      println(s"Unable to read $filepath : $exception")
      sys.exit(1)
    case Success(value) => value

def readInputAsMatrix[T <: Any](filepath: String, transform: Char => T = identity): Vector[Vector[T]] =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().map(_.map(transform).toVector).toVector
  } match
    case Failure(exception) =>
      println(s"Unable to read $filepath : $exception")
      sys.exit(1)
    case Success(value) => value

class ParseException(input: String, parser: String = "noname") extends Exception

trait Problem

abstract class ProblemParser[A] extends RegexParsers:

  def parse(source: BufferedSource): A =
    val input = source.mkString
    parse(root, input) match
      case Success(result, next) if next.atEnd => result
      case _                                   => throw new ParseException(input, "problem")

  def root: Parser[A]

  def integer: Parser[Int] = """(\d+)""".r ^^ { value => value.toInt }

  def nl: Parser[Unit] = "\n".r ^^^ { () }
