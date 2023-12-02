package day03

import scala.util.Using
import scala.util.control.NonLocalReturns

case class Rucksack(left: String, right: String)

object Rucksack:
  def fromString(string: String): Rucksack =
    val (left, right) = string.splitAt(string.length() / 2)
    Rucksack(left, right)

def priority(item: Char): Int = item match {
  case c if c.toInt >= 'a'.toInt && c.toInt <= 'z'.toInt =>
    c.toInt - 'a'.toInt + 1
  case c if c.toInt >= 'A'.toInt && c.toInt <= 'Z'.toInt =>
    c.toInt - 'A'.toInt + 27
  case _ =>
    sys.exit(1)
}

@main def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath))(source =>
    source.getLines().toList
  ).getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val score1: Int = input.map { line =>
    val (left, right) = line.splitAt(line.length() / 2)
    val duplicate = left.toList.intersect(right.toList).head
    priority(duplicate)
  }.sum

  val score2: Int = input
    .grouped(3)
    .map { 
      case List(a, b, c) =>
      val badge = a.toList.intersect(b.toList.intersect(c.toList)).head
      priority(badge)
    }
    .sum

  println(s"======== Day 03 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
