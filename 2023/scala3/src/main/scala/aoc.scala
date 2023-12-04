package aoc

import scala.util.Using
import scala.util.parsing.combinator.Parsers

def readInput(filepath: String) =
  Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse {
    println("Unable to read $filepath")
    sys.exit(1)
  }

class ParseException(input : String, parser : String = "noname") extends Exception
