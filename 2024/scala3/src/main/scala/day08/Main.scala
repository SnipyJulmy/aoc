package day08

import scala.collection.mutable

@main
def main(filepath: String): Unit =

  val input  = aoc.readInputAsMatrix(filepath)
  val score1 = antinodes(input).map(_.count(_ == '#')).sum
  val score2 = antinodesWithHarmonicResonant(input).map(_.count(_ == '#')).sum

  println(s"${"=" * 8} Day 08 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def antinodes(map: Vector[Vector[Char]]): Vector[Vector[Char]] =
  val cols      = map.length
  val rows      = map.head.length
  val groups    = antennaGroups(map)
  var antinodes = Array.fill(rows, cols)('.')
  groups.foreach { antennas =>
    antennas.combinations(2).foreach {
      case List((x1, y1), (x2, y2)) =>
        val vx = x1 - x2
        val x3 = x1 + vx
        val x4 = x2 - vx
        val vy = y1 - y2
        val y3 = y1 + vy
        val y4 = y2 - vy
        if x3 >= 0 && x3 < rows && y3 >= 0 && y3 < cols then antinodes(x3)(y3) = '#'
        if x4 >= 0 && x4 < rows && y4 >= 0 && y4 < cols then antinodes(x4)(y4) = '#'
      case _ => ()
    }
  }
  antinodes.map(_.toVector).toVector

def antinodesWithHarmonicResonant(map: Vector[Vector[Char]]): Vector[Vector[Char]] =
  val cols      = map.length
  val rows      = map.head.length
  val groups    = antennaGroups(map)
  var antinodes = Array.fill(rows, cols)('.')
  groups.foreach { antennas =>
    antennas.combinations(2).foreach {
      case List((x1, y1), (x2, y2)) =>
        val vx = x1 - x2
        val vy = y1 - y2

        var t  = 0
        var x3 = x1 + vx * t
        var y3 = y1 + vy * t
        while x3 >= 0 && x3 < rows && y3 >= 0 && y3 < cols do
          antinodes(x3)(y3) = '#'
          t += 1
          x3 = x1 + vx * t
          y3 = y1 + vy * t

        t = 0
        var x4 = x2 - vx * t
        var y4 = y2 - vy * t
        while x4 >= 0 && x4 < rows && y4 >= 0 && y4 < cols do
          antinodes(x4)(y4) = '#'
          t += 1
          x4 = x2 - vx * t
          y4 = y2 - vy * t
      case _ => ()
    }
  }
  antinodes.map(_.toVector).toVector

def antennaGroups(map: Vector[Vector[Char]]): List[List[(Int, Int)]] =
  val antennaGroups: mutable.HashMap[Char, List[(Int, Int)]] = new mutable.HashMap()
  for {
    i <- map.indices
    j <- map(i).indices
    if map(i)(j) != '.'
  } {
    antennaGroups.updateWith(map(i)(j)) {
      case None         => Some(List((i, j)))
      case Some(points) => Some((i, j) :: points)
    }
  }
  antennaGroups.values.toList
