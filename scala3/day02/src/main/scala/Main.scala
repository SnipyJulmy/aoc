import scala.util.Using

enum Shape(val score: Int):
  case Rock extends Shape(1)
  case Paper extends Shape(2)
  case Scissor extends Shape(3)

object Shape:
  def fromString(string: String): Shape = string match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissor
    case _ =>
      println(s"Unknown shape input : $string")
      sys.exit(1)
  }

enum Outcome(val score: Int):
  case Lost extends Outcome(0)
  case Draw extends Outcome(3)
  case Win extends Outcome(6)

object Outcome:
  def fromString(string : String): Outcome = string match {
    case "X" => Lost
    case "Y" => Draw
    case "Z" => Win
  }

type Score = Int

import Shape._
import Outcome._

def play(a: Shape, b: Shape): Score = (a, b) match {
  case (Rock, Paper)    => Outcome.Win.score + b.score
  case (Rock, Scissor)  => Outcome.Lost.score + b.score
  case (Paper, Rock)    => Outcome.Lost.score + b.score
  case (Paper, Scissor) => Outcome.Win.score + b.score
  case (Scissor, Rock)  => Outcome.Win.score + b.score
  case (Scissor, Paper) => Outcome.Lost.score + b.score
  case _                => Outcome.Draw.score + b.score
}

def solve(shape : Shape, outcome : Outcome): Score = 
  val solution = (shape, outcome) match {
    case (Rock, Lost) => Scissor
    case (Rock, Draw) => Rock
    case (Rock, Win) => Paper
    case (Paper, Lost) => Rock
    case (Paper, Draw) => Paper
    case (Paper, Win) => Scissor
    case (Scissor, Lost) => Paper
    case (Scissor, Draw) => Scissor
    case (Scissor, Win) => Rock
  }
  play(shape, solution)


@main def main(filepath: String): Unit =
  val input = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println("Unable to read $filepath"); sys.exit(1) }

  val score1 = input.map { line => 
    val Seq(a,b) = line.split(" ").toSeq.map(Shape.fromString)
    (a,b)
  }.foldLeft(0) { case (score, (a, b)) => score + play(a, b) }

  val score2 = input.map { line =>
    val Seq(a,b) = line.split(" ").toSeq
    (Shape.fromString(a), Outcome.fromString(b))
  }.foldLeft(0) { case (score, (a, o)) => score + solve(a, o)}

  println(s"Score part 1 : $score1")

  println(s"Score part 2 : $score2")
