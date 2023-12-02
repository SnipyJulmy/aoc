package day09

import scala.util.Using

case class Pos(x: Int, y: Int):
  lazy val u: Pos = Pos(x, y + 1)
  lazy val d: Pos = Pos(x, y - 1)
  lazy val r: Pos = Pos(x + 1, y)
  lazy val l: Pos = Pos(x - 1, y)

  def chebyshevDistance(that: Pos) = math.max(diffX(that).abs, diffY(that).abs)
  def diffX(that: Pos)             = this.x - that.x
  def diffY(that: Pos)             = this.y - that.y

  def move(movement: Movement): Pos = movement match
    case Movement.Left  => l
    case Movement.Right => r
    case Movement.Down  => d
    case Movement.Up    => u

object Pos:
  val origin: Pos = Pos(0, 0)

type Rope = List[Pos]

object Rope:
  def of(length: Int) = List.fill(length)(Pos.origin)

extension (rope: Rope)
  def u: Rope = move(Movement.Up, rope)
  def d: Rope = move(Movement.Down, rope)
  def r: Rope = move(Movement.Right, rope)
  def l: Rope = move(Movement.Left, rope)

def arrange(rope: Rope): Rope = rope match
  case Nil            => Nil
  case _ :: Nil       => rope
  case k1 :: k2 :: xs => k1 :: arrange(fix(k1)(k2) :: xs)

// fix k2 position w.r.t k1
def fix(k1: Pos)(k2: Pos): Pos = (k1.diffX(k2), k1.diffY(k2)) match
  case n if (for (x <- -1 to 1; y <- -1 to 1) yield (x, y)).contains(n) => k2
  case (-2, 0)                                                          => k2.l
  case (2, 0)                                                           => k2.r
  case (0, 2)                                                           => k2.u
  case (0, -2)                                                          => k2.d
  case (-2, 1)                                                          => k2.l.u
  case (2, 1)                                                           => k2.r.u
  case (-2, -1)                                                         => k2.l.d
  case (2, -1)                                                          => k2.r.d
  case (1, 2)                                                           => k2.r.u
  case (1, -2)                                                          => k2.r.d
  case (-1, 2)                                                          => k2.l.u
  case (-1, -2)                                                         => k2.l.d
  case (2, 2)                                                           => k2.r.u
  case (2, -2)                                                          => k2.r.d
  case (-2, 2)                                                          => k2.l.u
  case (-2, -2)                                                         => k2.l.d
  case (x, y)                                                           => throw new IllegalStateException(s"($x,$y)")

def move(movement: Movement, rope: Rope): Rope = rope match
  case Nil      => Nil
  case x :: Nil => x.move(movement) :: Nil
  case x :: xs  => arrange(x.move(movement) :: xs)

case class State(head: Pos, tail: Pos):
  def move(movement: Movement): State =
    val nextHead = movement match
      case Movement.Left  => head.l
      case Movement.Right => head.r
      case Movement.Down  => head.d
      case Movement.Up    => head.u
    if nextHead.chebyshevDistance(tail) < 2 then State(nextHead, tail) else State(nextHead, head)

object State:
  val origin: State = State(Pos.origin, Pos.origin)

enum Movement:
  case Left, Right, Down, Up

def parse(line: String): List[Movement] =
  val Array(lhs, rhs) = line.split(" ")
  val movement = lhs match
    case "R" => Movement.Right
    case "L" => Movement.Left
    case "U" => Movement.Up
    case "D" => Movement.Down
    case _   => throw new IllegalArgumentException("Unknown movement $lhs")
  val amount = rhs.toInt
  List.fill(amount)(movement)

def simulate(movements: List[Movement], ropeLength: Int): (Rope, Set[Pos]) =
  val rope = Rope.of(ropeLength)
  movements
    .foldLeft((rope, Set(Pos.origin))) { case ((rope, visitedPos), movement) =>
      val nextRopeState = move(movement, rope)
      (nextRopeState, visitedPos + nextRopeState.last)
    }

def simulate(movements: List[Movement]): (State, Set[Pos]) =
  movements
    .foldLeft((State.origin, Set(Pos.origin))) { case ((state, visitedPos), movement) =>
      val nextState @ State(head, tail) = state.move(movement)
      (nextState, visitedPos + tail)
    }

def show(rope: Rope): Unit =
  val minX = rope.map(_.x).min
  val maxX = rope.map(_.x).max
  val minY = rope.map(_.y).min
  val maxY = rope.map(_.y).max

  println(s"($minX,$minY)($maxX,$maxY),$rope")

  for (y <- maxY to minY by -1) {
    for (x <- minX to maxX by 1) {
      val idx = rope.indexOf(Pos(x, y))
      if idx >= 0 then print(s"$idx") else print(".")
    }
    println()
  }

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val movements = input.flatMap(parse)
  val score1    = simulate(movements)._2.size
  val score1b   = simulate(movements, 2)._2.size
  val score2    = simulate(movements, 10)._2.size

  println(s"======== Day 09 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 1b : $score1b")
  println(s"Score part 2  : $score2")
  println("=" * 24)
