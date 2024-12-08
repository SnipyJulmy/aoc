package day07

import scala.annotation.tailrec

@main
def main(filepath: String): Unit =

  val lines = aoc.readInput(filepath)
  val equations = lines.map { line =>
    val Array(result, numbers) = line.trim().split(":")
    (BigInt(result), numbers.trim().split(" ").toList)
  }

  val score1 = equations
    .map { case (result, numbers) =>
      solve(result, numbers.map(BigInt(_))).getOrElse(BigInt(0))
    }
    .reduce(_ + _)
  val score2 = equations
    .map { case (result, numbers) =>
      solveWithConcat(result, numbers).getOrElse(BigInt(0))
    }
    .reduce(_ + _)

  println(s"${"=" * 8} Day 07 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def solve(result: BigInt, numbers: List[BigInt]): Option[BigInt] =

  def inner(acc: BigInt, numbers: List[BigInt]): Option[BigInt] = numbers match
    case Nil => if acc == result then Some(result) else None
    case x :: xs =>
      inner(acc + x, xs) orElse inner(acc * x, xs)

  inner(numbers.head, numbers.tail)

def solveWithConcat(result: BigInt, numbers: List[String]): Option[BigInt] =

  def inner(acc: String, numbers: List[String]): Option[BigInt] =
    numbers match
      case Nil => if BigInt(acc) == result then Some(result) else None
      case x :: xs =>
        inner(plus(acc, x), xs) orElse
          inner(times(acc, x), xs) orElse
          inner(concat(acc, x), xs)

  inner(numbers.head, numbers.tail)

def plus(a: String, b: String): String   = (BigInt(a) + BigInt(b)).toString
def times(a: String, b: String): String  = (BigInt(a) * BigInt(b)).toString
def concat(a: String, b: String): String = a + b
