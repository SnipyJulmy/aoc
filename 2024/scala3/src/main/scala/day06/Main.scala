package day06

import scala.util.control.Breaks.{break, breakable}
import Direction._

@main
def main(filepath: String): Unit =

  val input            = aoc.readInputAsMatrix(filepath).toArray.map(_.toArray)
  val (score1, score2) = simulate(input)

  println(s"${"=" * 8} Day 06 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def simulate(start: Array[Array[Char]]): (Int, Int) =
  val maxX    = start.length
  val maxY    = start.head.length
  val visited = start.map(_.map(_ => false))

  var Seq((x, y)) = for {
    x <- start.indices
    y <- start.head.indices
    if start(x)(y) == '^'
  } yield (x, y)
  var direction = N
  visited(x)(y) = true

  var count            = 1
  var possibleObstacle = 0

  breakable {
    while true do
      val (x1, y1) = direction((x, y))
      if x1 >= 0 && x1 < maxY && y1 >= 0 && y1 < maxX then

        if start(x1)(y1) != '#' then
          start(x1)(y1) = '#'
          if (isLooping(start, x, y, direction)) then possibleObstacle += 1
          start(x1)(y1) = '.'

        if start(x1)(y1) == '#' then direction = direction.rotateRight
        else
          start(x)(y) = '.'
          x = x1
          y = y1
          if !visited(x)(y) then
            visited(x)(y) = true
            count += 1
      else break
  }

  (count, possibleObstacle)

// check if the given map simulation has a loop or not
def isLooping(map: Array[Array[Char]], x: Int, y: Int, direction: Direction): Boolean =
  val maxX    = map.length
  val maxY    = map.head.length
  val visited = map.map(_.map(_ => false))
  var xStep   = x
  var yStep   = y
  var dir     = direction

  breakable {
    while true do
      val (xNext, yNext) = direction((xStep, yStep))
      if xNext >= 0 && xNext < maxX && yNext >= 0 && yNext < maxY then
        if map(xNext)(yNext) == '#' then dir = dir.rotateRight
        else
          xStep = xNext
          yStep = yNext
          visited(xStep)(yStep) = true
        if xStep == x && yStep == y && dir == direction then break
      else break
  }

  return xStep == x && yStep == y && dir == direction

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

  lazy val char: Char = this match
    case N => '^'
    case S => 'v'
    case E => '>'
    case W => '<'
