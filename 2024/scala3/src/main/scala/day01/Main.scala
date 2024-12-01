package day01

import scala.util.Using

@main
def main(filepath: String): Unit =

  val input            = aoc.readInput(filepath)
  val List(ids1, ids2) = input.map(str => str.split(" +").map(_.toInt).toList).transpose
  val frequency        = ids2.groupBy(identity).mapValues(_.length)

  val score1 = (ids1.sorted zip ids2.sorted).map((a, b) => math.abs(a - b)).sum
  val score2 = ids1.foldLeft(0) { (similarity, id) =>
    similarity + id * frequency.getOrElse(id, 0)
  }

  println(s"${"=" * 8} Day 01 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
