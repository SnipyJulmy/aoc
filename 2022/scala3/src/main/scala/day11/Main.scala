package day11

import scala.util.Using
import scala.collection.mutable

type WorryLevel = BigInt

case class Monkey(items: mutable.Queue[WorryLevel], operation: Operation, predicate: ModPredicate):
  var nbInspectedItems: BigInt = 0

  def inspect(): WorryLevel =
    nbInspectedItems += 1
    items.dequeue()

enum Operation:
  case Add(lhs: Expr, rhs: Expr)
  case Mul(lhs: Expr, rhs: Expr)

  def compute(old: WorryLevel): WorryLevel = this match
    case Operation.Add(lhs, rhs) => lhs.value(old) + rhs.value(old)
    case Operation.Mul(lhs, rhs) => lhs.value(old) * rhs.value(old)

enum Expr:
  case Number(value: WorryLevel)
  case Old

  def value(old: WorryLevel): WorryLevel = this match
    case Number(value) => value
    case Old           => old

object Expr:
  def from(string: String): Expr =
    if (string == "old") then Expr.Old else Expr.Number(string.toInt)

case class ModPredicate(mod: WorryLevel, ifTrue: Int, ifFalse: Int):
  def test(worry: WorryLevel): Int = this match
    case ModPredicate(mod, ifTrue, ifFalse) => if worry % mod == 0 then ifTrue else ifFalse

def parse(lines: List[String]): Monkey =

  val numberRegex = raw"(\d+)".r

  val startingItems: List[WorryLevel] = numberRegex.findAllIn(lines(1)).toList.map(_.toLong)
  val operation: Operation =
    val values = lines(2).split(" ")
    val slice  = values.slice(values.length - 3, values.length)
    slice match
      case Array(lhs, "+", rhs) => Operation.Add(Expr.from(lhs), Expr.from(rhs))
      case Array(lhs, "*", rhs) => Operation.Mul(Expr.from(lhs), Expr.from(rhs))

  val action: ModPredicate =
    val mod     = numberRegex.findAllIn(lines(3)).toList.map(_.toInt).head
    val ifTrue  = numberRegex.findAllIn(lines(4)).toList.map(_.toInt).head
    val ifFalse = numberRegex.findAllIn(lines(5)).toList.map(_.toInt).head
    ModPredicate(mod, ifTrue, ifFalse)
  Monkey(mutable.Queue.from(startingItems), operation, action)

def lcm(xs: Iterable[WorryLevel]): WorryLevel     = xs.reduceLeft((a, b) => lcm(a, b))
def lcm(a: WorryLevel, b: WorryLevel): WorryLevel = a * b / gcd(a, b)
def gcd(a: WorryLevel, b: WorryLevel): WorryLevel = if b > a then gcd(b, a) else if b == 0 then a else gcd(b, a % b)

def simulate(worryFactor: WorryLevel)(monkeys: Vector[Monkey], nbRounds: Int): Vector[Monkey] =
  val monkeysLcm = lcm(monkeys.map(_.predicate.mod))
  println(s"lcm(${monkeys.map(_.predicate.mod).mkString(",")}) = $monkeysLcm")
  for (round <- 1 to nbRounds) {
    monkeys.foreach { monkey =>
      while (monkey.items.nonEmpty) {
        val item   = monkey.inspect()
        val worry  = monkey.operation.compute(item) % monkeysLcm
        val borred = worry / worryFactor
        val target = monkey.predicate.test(borred)
        monkeys(target).items.enqueue(borred)
      }
    }
  }
  monkeys

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val m1 = simulate(3)(input.filter(_.nonEmpty).grouped(6).map(_.map(_.trim())).map(parse).toVector, 20)
  val m2 = simulate(1)(input.filter(_.nonEmpty).grouped(6).map(_.map(_.trim())).map(parse).toVector, 10000)
  println(m2.zipWithIndex.map { case (monkey, idx) => s"Monkey $idx -> ${monkey.nbInspectedItems} : ${monkey.items.mkString(", ")}" } mkString "\n")

  val score1 = m1.map(_.nbInspectedItems).sorted.reverse.take(2).product
  val score2 = m2.map(_.nbInspectedItems).sorted.reverse.take(2).product

  println(s"======== Day 11 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
