package day06

import Direction._
import scala.annotation.tailrec
import scala.collection.mutable

@main
def main(filepath: String): Unit =

  val input       = aoc.readInputAsMatrix(filepath)
  val (score1, _) = simulate(input)
  val score2      = countLoopingObstacle(input)

  println(s"${"=" * 8} Day 06 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def simulate(map: Vector[Vector[Char]]): (Int, Int) =
  val maxX    = map.length
  val maxY    = map.head.length
  val visited = Array.fill(maxX, maxY)(false)
  val loops   = Array.fill(maxX, maxY)(false)

  val Seq((xStart, yStart)) = for {
    x <- map.indices
    y <- map.head.indices
    if map(x)(y) == '^'
  } yield (x, y)

  def inner(x: Int, y: Int, dir: Direction): Unit =
    visited(x)(y) = true
    val (xNext, yNext) = dir((x, y))
    if xNext >= 0 && xNext < maxX && yNext >= 0 && yNext < maxY then
      if map(xNext)(yNext) == '#' then inner(x, y, dir.rotateRight)
      else
        if isLooping(map.updated(xNext, map(xNext).updated(yNext, '#')), x, y, dir) then loops(xNext)(yNext) = true
        inner(xNext, yNext, dir)

  inner(xStart, yStart, N)
  loops(xStart)(yStart) = false

  (visited.map(_.count(identity)).sum, loops.map(_.count(identity)).sum)

def countLoopingObstacle(map: Vector[Vector[Char]]): Int =
  val Seq((xStart, yStart)) = for {
    x <- map.indices
    y <- map.head.indices
    if map(x)(y) == '^'
  } yield (x, y)
  var count = 0
  for (i <- map.indices) {
    for (j <- map(i).indices) {
      map(i)(j) match
        case '.' =>
          if isLooping(map.updated(i, map(i).updated(j, '#')), xStart, yStart, N) then count += 1
        case _ => ()
    }
  }
  count

// check if the given map has a loop or not
def isLooping(map: Vector[Vector[Char]], startX: Int, startY: Int, startDirection: Direction): Boolean =
  val maxX    = map.length
  val maxY    = map.head.length
  val visited = new mutable.HashSet[(Int, Int, Direction)]()

  @tailrec
  def inner(x: Int, y: Int, dir: Direction): Boolean =
    if visited.contains((x, y, dir)) then true
    else
      visited.add((x, y, dir))
      val (xNext, yNext) = dir((x, y))
      if xNext < 0 || xNext >= maxX || yNext < 0 || yNext >= maxY then false
      else if map(xNext)(yNext) == '#' then inner(x, y, dir.rotateRight)
      else inner(xNext, yNext, dir)

  inner(startX, startY, startDirection)

enum Direction(val f: ((Int, Int)) => (Int, Int)):
  case N extends Direction((x, y) => (x - 1, y))
  case S extends Direction((x, y) => (x + 1, y))
  case E extends Direction((x, y) => (x, y + 1))
  case W extends Direction((x, y) => (x, y - 1))

  def apply(point: (Int, Int)): (Int, Int) = this.f(point)

  lazy val rotateRight: Direction = this match
    case N => E
    case S => W
    case E => S
    case W => N

  override def toString(): String = this match
    case N => "^"
    case S => "v"
    case E => ">"
    case W => "<"
