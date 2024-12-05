package day05

import aoc.ProblemParser
import scala.collection.mutable

@main
def main(filepath: String): Unit =

  val problem = aoc.readInput(filepath, Parser)
  val score1 = problem.updates
    .filter(isCorrect(problem.orderingRules))
    .map(update => update((update.length - 1) / 2))
    .sum
  val score2 = problem.updates
    .filterNot(isCorrect(problem.orderingRules))
    .map(fix(problem.orderingRules))
    .map(update => update((update.length - 1) / 2))
    .sum

  println(s"${"=" * 8} Day 05 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def isCorrect(orderingRules: List[(Int, Int)])(update: List[Int]): Boolean =
  val indexedUpdate = update.zipWithIndex
  orderingRules.forall { case (a, b) =>
    (for {
      x <- indexedUpdate.find(_._1 == a).map(_._2)
      y <- indexedUpdate.find(_._1 == b).map(_._2)
    } yield (x < y)).getOrElse(true)
  }

def fix(orderingRules: List[(Int, Int)])(update: List[Int]): List[Int] =
  val list   = mutable.ListBuffer.from(update)
  var change = true
  while change do
    change = false
    orderingRules.foreach { case (a, b) =>
      if list.contains(a) && list.contains(b) then
        if list.indexOf(a) > list.indexOf(b) then
          val elt = list.remove(list.indexOf(b))
          list.insert(list.indexOf(a) + 1, elt)
          change = true
    }
  list.toList

def isBefore(orderingRules: List[(Int, Int)])(a: Int, b: Int): Boolean =
  orderingRules.contains((a, b))

case class Problem(
    orderingRules: List[(Int, Int)],
    updates: List[List[Int]]
)

object Parser extends ProblemParser[Problem]:
  override def skipWhitespace: Boolean = false
  def root: Parser[Problem]            = rep(orderingRule) ~ (nl ~> rep(update)) ^^ { x => Problem(x._1, x._2) }
  def orderingRule: Parser[(Int, Int)] = integer ~ ('|' ~> integer) <~ nl ^^ { case a ~ b => (a, b) }
  def update: Parser[List[Int]]        = integer ~ rep(',' ~> integer) <~ nl ^^ { case x ~ xs => x :: xs }
