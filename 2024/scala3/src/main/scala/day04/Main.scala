package day04

import Direction._

@main
def main(filepath: String): Unit =

  val input  = aoc.readInputAsMatrix(filepath)
  val score1 = countWord("XMAS".toVector, input)
  val score2 = countWordInX("MAS".toVector, input)

  println(s"${"=" * 8} Day 04 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def countWord(word: Vector[Char], grid: Vector[Vector[Char]]): Int =
  val rows = grid.length
  val cols = grid.head.length

  def innerSearch = search(word, grid)

  var count = 0
  for {
    i <- 0 until rows
    j <- 0 until cols
  } {
    if innerSearch(N, i, j) then count += 1
    if innerSearch(S, i, j) then count += 1
    if innerSearch(E, i, j) then count += 1
    if innerSearch(W, i, j) then count += 1
    if innerSearch(NE, i, j) then count += 1
    if innerSearch(NW, i, j) then count += 1
    if innerSearch(SE, i, j) then count += 1
    if innerSearch(SW, i, j) then count += 1
  }
  count

def countWordInX(word: Vector[Char], grid: Vector[Vector[Char]]): Int =
  val rows = grid.length
  val cols = grid.head.length

  def innerSearch = search(word, grid)

  var count = 0
  for {
    y <- 0 until rows
    x <- 0 until cols
  } {
    if innerSearch(SE, x - 1, y + 1) || innerSearch(NW, x + 1, y - 1) then
      if innerSearch(SW, x + 1, y + 1) || innerSearch(NE, x - 1, y - 1) then count += 1
  }
  count

def search(word: Vector[Char], grid: Vector[Vector[Char]])(direction: Direction, x: Int, y: Int): Boolean =
  val rows = grid.length
  val cols = grid.head.length
  def inner(coordinate: (Int, Int), idx: Int): Boolean =
    if idx >= word.length then true
    else if coordinate._1 >= 0 && coordinate._1 < rows && coordinate._2 >= 0 && coordinate._2 < cols then
      grid(coordinate._1)(coordinate._2) == word(idx) && inner(direction.f(coordinate), idx + 1)
    else false
  inner((x, y), 0)

enum Direction(val f: ((Int, Int)) => (Int, Int)):
  case N  extends Direction((x, y) => (x, y + 1))
  case S  extends Direction((x, y) => (x, y - 1))
  case E  extends Direction((x, y) => (x + 1, y))
  case W  extends Direction((x, y) => (x - 1, y))
  case NE extends Direction((x, y) => (x + 1, y + 1))
  case NW extends Direction((x, y) => (x - 1, y + 1))
  case SE extends Direction((x, y) => (x + 1, y - 1))
  case SW extends Direction((x, y) => (x - 1, y - 1))
