package day01

import scala.util.Using

@main
def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath))(source => source.getLines().toList).getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val score1 = input.map(calibration).sum
  val score2 = input.map(calibrationWithLetters).sum

  println(s"${"=" * 8} Day 01 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def calibration(line: String): Int =
  val c1 = line
    .find(_.isDigit)
    .getOrElse(throw IllegalArgumentException(s"wrong line input : $line"))
  val c2 = line.reverse
    .find(_.isDigit)
    .getOrElse(throw IllegalArgumentException(s"wrong line input : $line"))
  s"$c1$c2".toInt

def calibrationWithLetters(line: String): Int =
  val regex   = raw"(?=(1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine))".r
  val matches = regex.findAllIn(line).matchData.map(_.group(1)).toList
  val value = s"${toInt(matches.head)}${toInt(matches.last)}".toInt
  println(s"$line : $matches =>  $value")
  value

def toInt(str: String): String = str match
  case "one"   => "1"
  case "two"   => "2"
  case "three" => "3"
  case "four"  => "4"
  case "five"  => "5"
  case "six"   => "6"
  case "seven" => "7"
  case "eight" => "8"
  case "nine"  => "9"
  case digit   => digit
