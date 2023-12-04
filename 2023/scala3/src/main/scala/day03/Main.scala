package day03

@main
def main(filepath: String): Unit =

  val input = aoc.readInput(filepath)

  val digits = input.zipWithIndex.flatMap { case (line, row) =>
    val matches = """(\d+)""".r.findAllMatchIn(line)
    matches.toList.map(m => Digit(m.matched.toInt, row, m.start, m.end - 1))
  }

  val symbols = input.zipWithIndex.flatMap { case (line, row) =>
    val matches = """[^\.0-9]""".r.findAllMatchIn(line)
    matches.toList.map(m => Symbol(m.matched, row, m.start))
  }

  val gearParts = symbols.filter(_.value == "*").map(s => s -> parts(s, digits)).filter(_._2.length == 2)

  val score1 = digits.filter(d => symbols.exists(s => areTouching(s, d))).map(_.value).sum
  val score2 = gearParts.map(_._2.map(_.value).product).sum

  println(s"${"=" * 8} Day 03 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

def parts(symbol: Symbol, digits: List[Digit]): List[Digit] =
  digits.filter(d => areTouching(symbol, d))

def areTouching(s: Symbol, d: Digit): Boolean =
  (d.row - s.row).abs <= 1 && s.col >= d.start - 1 && s.col <= d.end + 1

case class Digit(value: Int, row: Int, start: Int, end: Int)
case class Symbol(value: String, row: Int, col: Int)
