package day13

class Day13Spec extends munit.FunSuite:

  val parser = new PacketParser

  test("compare") {
    assertEquals(compare(Atom.AList(List(Atom.Number(1))), Atom.AList(List(Atom.Number(0)))), 1)
    assertEquals(compare(Atom.AList(List(Atom.Number(1))), Atom.AList(List(Atom.Number(1)))), 0)
    assertEquals(compare(Atom.AList(List(Atom.Number(1))), Atom.AList(List(Atom.Number(2)))), -1)
    assertEquals(compare(parser.parseList("[]"), parser.parseList("[]")), 0)
    assertEquals(compare(parser.parseList("[1]"), parser.parseList("[0]")), 1)
    assertEquals(compare(parser.parseList("[1]"), parser.parseList("[1]")), 0)
    assertEquals(compare(parser.parseList("[1]"), parser.parseList("[2]")), -1)
    assertEquals(compare(parser.parseList("[1,2,3,4]"), parser.parseList("[1,2,3,4]")), 0)

    assertEquals(compare(parser.parseList("[[[[[[[[[1]]]]]]]]]"), parser.parseList("[1]")), 0)
    assertEquals(compare(parser.parseList("[1,1,3,1,1]"), parser.parseList("[1,1,5,1,1]")), -1)
    assertEquals(compare(parser.parseList("[7,7,7,7]"), parser.parseList("[7,7,7]")), -1)
  }
