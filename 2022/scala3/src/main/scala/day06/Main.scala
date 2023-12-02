package day06

import scala.util.Using

def findMarker(markerSize: Int)(stream: String): Option[Int] = {
  stream
    .sliding(markerSize)
    .zipWithIndex
    .find(_._1.toSet.size == markerSize)
    .map(_._2 + markerSize)
}

@main def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val stream = input.head

  val score1 = findMarker(4)(stream).get
  val score2 = findMarker(14)(stream).get

  println(s"======== Day 06 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
