package day14

import scala.util.Using

case class Pos(x: Int, y: Int):
  lazy val bot: Pos      = Pos(x, y + 1)
  lazy val botLeft: Pos  = Pos(x - 1, y + 1)
  lazy val botRight: Pos = Pos(x + 1, y + 1)

object Pos:
  def fromString(string: String): Pos =
    val Array(x, y) = string.split(",").map(_.toInt)
    Pos(x, y)

case class RockPath(path: List[Pos]):
  lazy val fullpath: List[Pos] =
    (path.zip(path.tail)).flatMap {
      case (Pos(x1, y1), Pos(x2, y2)) if x1 <= x2 && y1 <= y2 => for (x <- x1 to x2; y <- y1 to y2) yield Pos(x, y)
      case (Pos(x1, y1), Pos(x2, y2)) if x1 > x2 && y1 <= y2  => for (x <- x2 to x1; y <- y1 to y2) yield Pos(x, y)
      case (Pos(x1, y1), Pos(x2, y2)) if x1 <= x2 && y1 > y2  => for (x <- x1 to x2; y <- y2 to y1) yield Pos(x, y)
      case (Pos(x1, y1), Pos(x2, y2))                         => for (x <- x2 to x1; y <- y2 to y1) yield Pos(x, y)
    }

object RockPath:
  def fromString(string: String): RockPath = RockPath(string.split(" -> ").map(Pos.fromString).toList)

// return the amount of sand required to fill the given cavern from the given source
def fill(source: Pos, cavern: Set[Pos]): Int =
  val minX = cavern.map(_.x).min.min(source.x)
  val maxX = cavern.map(_.x).max.max(source.x)
  val minY = cavern.map(_.y).min.min(source.y)
  val maxY = cavern.map(_.y).max.max(source.y)

  def inner(cavern: Set[Pos], acc: Int): Int =
    fall(source, cavern) match
      case None       => acc
      case Some(rest) => inner(cavern + rest, acc + 1)

  def isInCavern(pos: Pos): Boolean = pos.x >= minX && pos.x <= maxX && pos.y >= minY && pos.y <= maxY

  // compute where the next unit of sand would rest in the given cavern input
  // return None if the unit of sand falls out of the cavern
  def fall(pos: Pos, cavern: Set[Pos]): Option[Pos] =
    if !isInCavern(pos) then None
    else if (!cavern.contains(pos.bot)) then fall(pos.bot, cavern)
    else if (!cavern.contains(pos.botLeft)) then fall(pos.botLeft, cavern)
    else if (!cavern.contains(pos.botRight)) then fall(pos.botRight, cavern)
    else if source == pos then None
    else if isInCavern(pos) then Some(pos)
    else None

  inner(cavern, 0)

def fillWithFloor(source: Pos, cavern: Set[Pos]): Int =

  val floor = cavern.map(_.y).max.max(source.y) + 2

  println(floor)

  def inner(cavern: Set[Pos], acc: Int): Int =
    fall(source, cavern) match
      case None       => acc + 1
      case Some(rest) => inner(cavern + rest, acc + 1)

  // compute where the next unit of sand would rest in the given cavern input
  // return None if the unit of sand falls out of the cavern
  def fall(pos: Pos, cavern: Set[Pos]): Option[Pos] =
    if (!cavern.contains(pos.bot) && pos.bot.y < floor) then fall(pos.bot, cavern)
    else if (!cavern.contains(pos.botLeft) && pos.botLeft.y < floor) then fall(pos.botLeft, cavern)
    else if (!cavern.contains(pos.botRight) && pos.botRight.y < floor) then fall(pos.botRight, cavern)
    else if source == pos then None
    else Some(pos)

  inner(cavern, 0)

def show(source: Pos, cavern: Set[Pos], withFloor: Boolean = false): Unit =
  val minX = cavern.map(_.x).min.min(source.x)
  val maxX = cavern.map(_.x).max.max(source.x)
  val minY = cavern.map(_.y).min.min(source.y)
  val maxY = cavern.map(_.y).max.max(source.y)
  for (y <- minY to maxY) {
    for (x <- minX to maxX) {
      if Pos(x, y) == source then print("+")
      else print(if cavern.contains(Pos(x, y)) then "#" else ".")
    }
    println()
  }

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val rockPaths: List[RockPath] = input.map(line => RockPath.fromString(line))
  val cavern: Set[Pos]          = rockPaths.flatMap(_.fullpath).toSet
  val start: Pos                = Pos(500, 0)
  val floorY                    = (cavern + start).map(_.y).max + 2

  val score1 = fill(start, cavern)
  val score2 = fillWithFloor(start, cavern)

  println(s"======== Day 14 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
