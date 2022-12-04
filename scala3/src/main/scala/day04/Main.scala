package day04

import scala.util.Using

@main def main(filepath: String): Unit =
  val lines = Using(scala.io.Source.fromFile(filepath))(source =>
    source.getLines().toList
  ).getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val ranges = lines.map { line =>
    val Array(l1,u1,l2,u2) = line.split(",|-").map(_.toInt)
    val r1 = l1 to u1
    val r2 = l2 to u2
    (r1,r2)
  }

  val score1: Int = ranges.count { case (r1,r2) =>
    val intersect = (r1 intersect r2)
    r1 == intersect || r2 == intersect
  }

  val score2: Int = ranges.count { case (r1,r2) =>
    (r1 intersect r2).nonEmpty
  }

  println(s"======== Day 04 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
