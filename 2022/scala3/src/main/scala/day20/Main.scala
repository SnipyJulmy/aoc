package day20

import scala.util.Using

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val score1 = 0
  val score2 = 0

  println(s"======== Day 16 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
