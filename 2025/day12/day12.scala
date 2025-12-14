//> using scala 3.3.6
//> using dep "org.scala-lang.modules::scala-parser-combinators:2.4.0"

import scala.util.parsing.combinator.RegexParsers
import java.io.BufferedReader
import java.io.FileReader
import scala.util.Using
import java.io.Reader
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex
import java.util.HashMap
import scala.annotation.tailrec
import scala.util.boundary, boundary.break

case class Shape(id: Int, bitboard: Array[Long], bitCount: Int):

  private def rotate(bb: Array[Long]) =
    val dst = new Array[Long](3)
    for y <- 0 to 2 do
      var row = 0L
      for x <- 0 to 2 do
        val src = (bb(2 - x) >> y) & 1L
        row |= (src << x)
      dst(y) = row
    dst

  private def flip(bb: Array[Long]) =
    val dst = new Array[Long](3)
    for y <- 0 to 2 do
      val r = bitboard(y)
      val b0 = (r & 1L) << 2
      val b1 = (r & 2L)
      val b2 = (r & 4L) >> 2
      dst(y) = b0 | b1 | b2
    dst

  lazy val variations: List[Shape] =
    val s0 = bitboard
    val s1 = rotate(s0)
    val s2 = rotate(s1)
    val s3 = rotate(s2)
    val f0 = flip(s0)
    val f1 = flip(s1)
    val f2 = flip(s2)
    val f3 = flip(s3)
    List(s0, s1, s2, s3, f0, f1, f2, f3)
      .map(_.toList)
      .distinct
      .map(bb => Shape(id, bb.toArray, bitCount))

case class Region(width: Int, height: Int, presents: List[Int])

class Parser extends RegexParsers:

  override protected val whiteSpace: Regex = """[ \t]+""".r

  def parse(reader: Reader): (List[Shape], List[Region]) =
    this.parse(problem, reader) match
      case Success(result, next) if next.atEnd => result
      case Success(result, next) =>
        throw RuntimeException(
          s"invalid input... : $result \n ${next.first}"
        )
      case Failure(msg, next) =>
        throw RuntimeException(s"invalid input... $msg $next")
      case _ =>
        throw RuntimeException(s"invalid input...")

  def problem: Parser[(List[Shape], List[Region])] =
    rep1(shape) ~ rep1(region) ^^ { case shapes ~ regions =>
      (shapes, regions)
    }

  def shape: Parser[Shape] =
    ("""\d+""".r <~ ":" ~ nl) ~ repN(3, """[#\.]{3}""".r <~ nl) ^^ {
      case id ~ shape =>
        val h = shape.length
        val w = if h > 0 then shape.head.length else 0
        val board = new Array[Long](h)
        for y <- shape.indices do
          var row = 0L
          for x <- shape(y).indices do
            if shape(y).charAt(x) == '#' then row |= (1L << x)
          board(y) = row
        Shape(id.toInt, board, shape.map(_.count(_ == '#')).sum)
    }

  def region: Parser[Region] =
    """\d+""".r ~ "x" ~ """\d+""".r ~ ":" ~ rep1("""\d+""".r) <~ nl ^^ {
      case width ~ _ ~ height ~ _ ~ ids =>
        Region(width.toInt, height.toInt, ids.map(_.toInt))
    }

  def nl = """(\r?\n)+""".r

class Bitboard(val width: Int, val height: Int):

  var emptyCells: Int = width * height

  private val cells = new Array[Long](height)
  private val mask = (1L << width) - 1L

  inline def free(shape: Shape, x: Int, y: Int): Boolean =
    if (x < 0 || y < 0 || y + 3 > height) then false
    else
      (cells(y) & (shape.bitboard(0) << x)) == 0L &&
      (cells(y + 1) & (shape.bitboard(1) << x)) == 0L &&
      (cells(y + 2) & (shape.bitboard(2) << x)) == 0L &&
      ((shape.bitboard(0) << x) & ~mask) == 0L &&
      ((shape.bitboard(1) << x) & ~mask) == 0L &&
      ((shape.bitboard(2) << x) & ~mask) == 0L

  inline def place(shape: Shape, x: Int, y: Int): Unit =
    cells(y) |= (shape.bitboard(0) << x)
    cells(y + 1) |= (shape.bitboard(1) << x)
    cells(y + 2) |= (shape.bitboard(2) << x)
    emptyCells -= shape.bitCount

  inline def remove(shape: Shape, x: Int, y: Int): Unit =
    cells(y) &= ~(shape.bitboard(0) << x)
    cells(y + 1) &= ~(shape.bitboard(1) << x)
    cells(y + 2) &= ~(shape.bitboard(2) << x)
    emptyCells += shape.bitCount

  inline def foreachFreeCell(inline block: (Int, Int) => Unit): Unit =
    var y = 0
    while y < height do
      val row = cells(y)
      if (row & mask) != mask then
        var freeSpots = ~row & mask
        while freeSpots != 0L do
          block(java.lang.Long.numberOfTrailingZeros(freeSpots), y)
          freeSpots &= (freeSpots - 1)
      y += 1

  override def toString(): String = pretty(cells, width, height)

def pretty(bitboard: Array[Long], width: Int, height: Int): String =
  val sb = StringBuilder()
  sb.append("+").append("-" * width).append("+\n")
  for y <- 0 until height do
    sb.append("|")
    val row = bitboard(y)
    for x <- 0 until width do
      val isOccupied = (row & (1L << x)) != 0
      sb.append(if isOccupied then '#' else ' ')
    sb.append("|\n")
  sb.append("+").append("-" * width).append("+")
  sb.toString

@main
def main(filepath: String): Unit =
  val parser = Parser()
  Using(BufferedReader(FileReader(filepath)))(parser.parse) match
    case Success((shapes, regions)) =>
      val map = shapes.map(shape => shape.id -> shape).toMap
      val size = regions.length
      val score1 = regions.zipWithIndex.count { (region, idx) =>
        println(s"solve for region $idx/$size")
        canFitPresent(map)(region)
      }
      val score2 = 1729 // ;)
      println(s"Part 1: $score1")
      println(s"Part 2: $score2")
    case e =>
      sys.exit(1)

def canFitPresent(shapesMap: Map[Int, Shape])(region: Region): Boolean =
  val board = Bitboard(region.width, region.height)
  val shapes = region.presents.zipWithIndex
    .flatMap { (count, idx) =>
      List.fill(count) { shapesMap(idx) }
    }
    .sortBy(-_.bitCount)
  val shapesVariations = shapes.map(s => s.variations)
  val shapeSize = shapes.foldLeft(0)((a, b) => a + b.bitCount)

  if board.emptyCells < shapeSize then false
  else solveVariations(board, shapesVariations)

def solveVariations(
    board: Bitboard,
    shapesVariations: List[List[Shape]]
): Boolean = shapesVariations match
  case Nil => true
  case variants :: remaining =>
    boundary:
      for (shape, idx) <- variants.zipWithIndex do
        board.foreachFreeCell { (x, y) =>
          if board.free(shape, x, y) then
            board.place(shape, x, y)
            if solveVariations(board, remaining) then break(true)
            board.remove(shape, x, y)
        }
      false
