package day01

import scala.util.Using

@main def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath))(source =>
    source.getLines().toList
  ).getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val caloriesByElf = input
    .foldLeft[List[Int]](List(0)) {
      case (Nil, elt) => List(elt.toInt)
      case (ls @ x :: xs, elt) =>
        if (elt.isEmpty()) 0 :: ls
        else (x + elt.toInt) :: xs
    }

  val score1 = caloriesByElf.max
  val score2 = caloriesByElf.sorted(Ordering.Int.reverse).take(3).sum

  println(s"======== Day 01 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
