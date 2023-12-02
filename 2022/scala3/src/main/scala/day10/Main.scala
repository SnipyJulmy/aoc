package day10

import scala.util.Using

enum Command:
  case Noop
  case Addx(value: Int)

def parse(line: String): Command = line match {
  case "noop" => Command.Noop
  case addx   => Command.Addx(addx.split(" ").last.toInt)
}

def signalHistory(commands: List[Command]): List[Int] =
  commands
    .foldLeft(List[Int](1)) {
      case (x, Command.Noop)        => x.head :: x
      case (x, Command.Addx(value)) => value + x.head :: x.head :: x
    }
    .reverse

def signalStregth(commands: List[Command], start: Int, period: Int, number: Int): Int =
  signalHistory(commands).zipWithIndex
    .map { case (x, y) => (x, y + 1) }
    .drop(start - 1)
    .grouped(period)
    .map(_.head)
    .take(number)
    .map { case (x, idx) => x * idx }
    .sum

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val commands = input.map(parse)

  val score1 = signalStregth(commands, 20, 40, 6)

  println(s"======== Day 10 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : ")

  signalHistory(commands)
    .grouped(40)
    .map { line =>
      line.zipWithIndex.map { case (sprite, pixel) => if (sprite - pixel).abs <= 1 then "#" else "." }
    }
    .foreach(line => println(line.mkString))

  println("=" * 24)
