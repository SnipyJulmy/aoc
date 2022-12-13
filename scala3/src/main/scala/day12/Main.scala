package day12

import scala.util.Using
import scala.collection.immutable.Queue

type Pos = (Int, Int)

extension (v: Vector[Vector[Int]])
  def posOf(elem: Int): (Int, Int) =
    (for {
      i <- v.indices
      j <- v(i).indices
      if v(i)(j) == elem
    } yield (i, j)).head

  def at(pos: (Int, Int)): Int = v(pos._1)(pos._2)

  def update(x: Int, y: Int, elem: Int) =
    v.updated(x, v(x).updated(y, elem))

extension (pos: (Int, Int))
  def top: (Int, Int)   = (pos._1, pos._2 + 1)
  def bot: (Int, Int)   = (pos._1, pos._2 - 1)
  def right: (Int, Int) = (pos._1 + 1, pos._2)
  def left: (Int, Int)  = (pos._1 - 1, pos._2)
  def adj: Seq[Pos]     = Seq(top, bot, right, left)

def bfs(heightMap: Vector[Vector[Int]])(start: Pos, end: Pos): Option[Int] =

  def inner(queue: Queue[(Pos, Int)], explored: Set[Pos]): Option[Int] =
    if queue.isEmpty then None
    else
      val ((crt, steps), rest) = queue.dequeue
      if crt == end then Some(steps)
      else
        val edgeToExplore = crt.adj.filterNot { case pos @ (x, y) =>
          x < 0 || y < 0 || x >= heightMap.length || y >= heightMap(x).length ||
          heightMap.at(pos) - heightMap.at(crt) > 1 || explored.contains(pos)
        }
        inner(rest.appendedAll(edgeToExplore.map(pos => (pos, steps + 1))), explored ++ edgeToExplore)

  inner(Queue(start -> 0), Set(start))

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val parsedMap: Vector[Vector[Int]] = input.map(line => line.toVector.map(_.toInt)).toVector
  val start @ (xStart, yStart)       = parsedMap.posOf('S'.toInt)
  val end @ (xEnd, yEnd)             = parsedMap.posOf('E'.toInt)

  val heightMap = parsedMap
    .update(xStart, yStart, 'a'.toInt)
    .update(xEnd, yEnd, 'z'.toInt)

  assert(heightMap.at(start) == 'a'.toInt)
  assert(heightMap.at(end) == 'z'.toInt)

  val score1 = bfs(heightMap)(start, end).get
  val score2 = (for {
    i <- 0 until heightMap.length
    j <- 0 until heightMap(i).length
    if heightMap(i)(j) == 'a'.toInt
  } yield bfs(heightMap)((i, j), end)).flatten.min

  println(s"======== Day 12 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
