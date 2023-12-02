package day13

import scala.util.Using
import scala.util.parsing.combinator.RegexParsers

enum Atom:
  case Number(value: Int)
  case AList(values: List[Atom])

  def compare(that: Atom): Int =

    def inner(as: List[Atom], bs: List[Atom]): Int = (as, bs) match
      case (Nil, Nil)     => 0
      case (a :: as, Nil) => 1
      case (Nil, b :: bs) => -1
      case (a :: as, b :: bs) =>
        a.compare(b) match
          case 0 => inner(as, bs)
          case n => n

    (this, that) match
      case (Number(a), Number(b))           => if a < b then -1 else if a > b then 1 else 0
      case (a: Number, bs: AList)           => AList(List(a)).compare(bs)
      case (as: AList, b: Number)           => as.compare(AList(List(b)))
      case (as @ AList(av), bs @ AList(bv)) => inner(av, bv)

def compare(a: Atom, b: Atom) = a.compare(b)

class PacketParser extends RegexParsers:
  def atom: Parser[Atom]          = list | number
  def list: Parser[Atom.AList]    = "[" ~> rep(atom <~ ",".?) <~ "]" ^^ { values => Atom.AList(values) }
  def number: Parser[Atom.Number] = """(\d+)""".r ^^ { value => Atom.Number(value.toInt) }

  def parseList(source: String): Atom.AList = parse(list, source) match
    case Success(result, next) if next.atEnd => result
    case Success(result, next)               => throw new Exception(s"Unable to parse $source")
    case Failure(msg, next)                  => throw new Exception(s"Unable to parse $source : $msg")
    case Error(msg, next)                    => throw new Exception(s"Unable to parse $source : $msg")

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val parser = new PacketParser

  val packets = input.filter(_.nonEmpty).map(parser.parseList)

  val div1 = Atom.AList(List(Atom.AList(List(Atom.Number(2)))))
  val div2 = Atom.AList(List(Atom.AList(List(Atom.Number(6)))))

  val score1 = packets.grouped(2).map(x => (x.head, x.tail.head)).map { case (a, b) => a.compare(b) }.zipWithIndex.filter(_._1 == -1).map(_._2 + 1).sum
  val score2 =
    val sorted = (div1 :: div2 :: packets).sortWith((a, b) => a.compare(b) < 0)
    (sorted.indexOf(div1) + 1) * (sorted.indexOf(div2) + 1)

  println(s"======== Day 13 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
