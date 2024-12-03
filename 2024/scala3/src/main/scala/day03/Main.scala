package day03

import State._

@main
def main(filepath: String): Unit =

  val stream = aoc.readInputAsStream(filepath)
  val score1 = stream
    .foldLeft((0, M, "", "")) { case ((sum, crtState, lhs, rhs), char) =>
      (crtState, char) match
        case (M, 'm')                                => (sum, U, lhs, rhs)
        case (U, 'u')                                => (sum, L, lhs, rhs)
        case (L, 'l')                                => (sum, OP, lhs, rhs)
        case (OP, '(')                               => (sum, LHS, lhs, rhs)
        case (LHS, d) if d.isDigit && lhs.length < 3 => (sum, LHS, lhs + char, rhs)
        case (LHS, ',')                              => (sum, RHS, lhs, rhs)
        case (RHS, d) if d.isDigit && rhs.length < 3 => (sum, RHS, lhs, rhs + char)
        case (RHS, ')')                              => (sum + lhs.toInt * rhs.toInt, M, "", "")
        case _                                       => (sum, M, "", "")
    }
    ._1
  val score2 = stream
    .foldLeft((0, M, "", "", true)) { case ((sum, crtState, lhs, rhs, enable), char) =>
      (crtState, char) match
        case (M, 'd')                                => (sum, O, lhs, rhs, enable)
        case (O, 'o')                                => (sum, OP, lhs, rhs, enable)
        case (OP, '(')                               => (sum, CP, lhs, rhs, enable)
        case (OP, 'n')                               => (sum, AP, lhs, rhs, enable)
        case (CP, ')')                               => (sum, M, "", "", true)
        case (AP, '\'')                              => (sum, T, lhs, rhs, enable)
        case (T, 't')                                => (sum, EOP, lhs, rhs, enable)
        case (EOP, '(')                              => (sum, ECP, lhs, rhs, enable)
        case (ECP, ')')                              => (sum, M, "", "", false)
        case (M, 'm')                                => (sum, U, lhs, rhs, enable)
        case (U, 'u')                                => (sum, L, lhs, rhs, enable)
        case (L, 'l')                                => (sum, LHS, lhs, rhs, enable)
        case (LHS, '(')                              => (sum, LHS, lhs, rhs, enable)
        case (LHS, d) if d.isDigit && lhs.length < 3 => (sum, LHS, lhs + char, rhs, enable)
        case (LHS, ',')                              => (sum, RHS, lhs, rhs, enable)
        case (RHS, d) if d.isDigit && rhs.length < 3 => (sum, RHS, lhs, rhs + char, enable)
        case (RHS, ')') => (sum + (if enable then lhs.toInt * rhs.toInt else 0), M, "", "", enable)
        case _          => (sum, M, "", "", enable)
    }
    ._1

  println(s"${"=" * 8} Day 03 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

enum State:
  case M, U, L, OP, CP, LHS, RHS, O, N, T, AP, EOP, ECP
