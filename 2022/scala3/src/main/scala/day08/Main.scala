package day08

import scala.util.Using

extension (v: Vector[Int])
  def countVisibleTree(height: Int): Int =
    def inner(list: List[Int], count: Int): Int = list match
      case Nil                    => count
      case x :: xs if x >= height => count + 1
      case x :: xs                => inner(xs, count + 1)
    inner(v.toList, 0)

def isVisible(treeRows: Vector[Vector[Int]], treeCols: Vector[Vector[Int]])(i: Int, j: Int): Boolean =
  if i == 0 || i == treeRows.length - 1 then true
  else if j == 0 || j == treeCols.length - 1 then true
  else {
    val height = treeRows(i)(j)
    treeRows(i).slice(0, j).forall(_ < height) ||
    treeRows(i).slice(j + 1, treeRows.length).forall(_ < height) ||
    treeCols(j).slice(0, i).forall(_ < height) ||
    treeCols(j).slice(i + 1, treeCols.length).forall(_ < height)
  }

def scenicScore(treeRows: Vector[Vector[Int]], treeCols: Vector[Vector[Int]])(row: Int, col: Int): Int =
  val height = treeRows(row)(col)
  val west   = treeRows(row).slice(0, col).reverse
  val east   = treeRows(row).slice(col + 1, treeRows.length)
  val north  = treeCols(col).slice(0, row).reverse
  val south  = treeCols(col).slice(row + 1, treeCols.length)
  val res    = east.countVisibleTree(height) * west.countVisibleTree(height) * north.countVisibleTree(height) * south.countVisibleTree(height)
  if res == 496650 then
    println(s"($row,$col)")
    println(east.countVisibleTree(height))
    println(west.countVisibleTree(height))
    println(north.countVisibleTree(height))
    println(south.countVisibleTree(height))
  res

def debug(name: String, v: Vector[Int]): Unit = println(s"$name -> ${v.mkString(",")}")

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val treeRows: Vector[Vector[Int]] = input.map(str => str.toVector.map(_.toString.toInt)).toVector
  val treeCols: Vector[Vector[Int]] = treeRows.transpose

  var nbVisibleTree = 0
  for {
    i <- 0 until treeRows.length
    j <- 0 until treeCols.length
  } if isVisible(treeRows, treeCols)(i, j) then nbVisibleTree += 1

  var bestScenicScore = 0
  for {
    i <- treeRows.indices
    j <- treeCols.indices
  } bestScenicScore = math.max(bestScenicScore, scenicScore(treeRows, treeCols)(i, j))

  treeRows.foreach(r => println(r.mkString(",")))

  val score1 = nbVisibleTree
  val score2 = bestScenicScore

  println(s"======== Day 08 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
