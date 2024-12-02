package day02

import scala.util.Using

@main
def main(filepath: String): Unit =

  val input   = aoc.readInput(filepath)
  val reports = input.map(_.split(" ").map(_.toInt).toList)
  val score1  = reports.count(isSafe)
  val score2  = reports.count(isDampenerSafe)

  println(s"${"=" * 8} Day 02 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def isSafe(report: List[Int]): Boolean =
  def inner(head: Int, tail: List[Int], increasing: Boolean): Boolean =
    tail match
      case x :: xs =>
        val diff = math.abs(x - head)
        diff <= 3 &&
        diff >= 1 &&
        (increasing && x > head || !increasing && x < head) &&
        inner(x, xs, increasing)
      case Nil => true
  if report.size < 2 then true
  else
    val head = report.head
    val tail = report.tail
    inner(head, tail, head < tail.head)

def isDampenerSafe(report: List[Int]): Boolean =
  isSafe(report) ||
    report.indices.map(idx => report.patch(idx, Nil, 1)).exists(isSafe)
