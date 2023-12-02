package day05

import scala.util.Using

type Crate = Char
type State = Array[List[Crate]]
case class Move(amount: Int, from: Int, to: Int)

def transitionOneByOne[A](state: Vector[List[A]], move: Move): Vector[List[A]] = {
  val from              = move.from - 1
  val to                = move.to - 1
  val (crates, newFrom) = state(from).splitAt(move.amount)
  val newTo             = crates.reverse ++ state(to)
  state
    .updated(from, newFrom)
    .updated(to, newTo)
}

def transitionMultiple[A](state: Vector[List[A]], move: Move): Vector[List[A]] = {
  val from              = move.from - 1
  val to                = move.to - 1
  val (crates, newFrom) = state(from).splitAt(move.amount)
  val newTo             = crates ++ state(to)
  state
    .updated(from, newFrom)
    .updated(to, newTo)
}

@main def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val nbStacks                 = (input.head.length() + 1) / 4
  val separation               = input.zipWithIndex.find(_._1.isEmpty()).map(_._2).get
  val (arrangement, movements) = input.filter(_.nonEmpty).splitAt(separation)

  val initState = arrangement
    .map { line =>
      line
        .grouped(4)
        .map(crate => if (crate.startsWith("[")) Some(crate.charAt(1)) else None)
        .toList
    }
    .transpose
    .map(_.flatten)
    .toVector

  val moves = movements.map { line =>
    val Array(_, a, _, f, _, t) = line.split(" ")
    Move(a.toInt, f.toInt, t.toInt)
  }

  val score1 = moves.foldLeft(initState)(transitionOneByOne).map(_.head).mkString
  val score2 = moves.foldLeft(initState)(transitionMultiple).map(_.head).mkString

  println(s"======== Day 05 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
