package day07

import scala.collection.mutable

class Day07Spec extends munit.FunSuite:
  test("parse") {
    val result   = parse(List("$ cd /"))
    val expected = List(TermOutput.Cd("/"))
    assertEquals(result, expected)
  }

  test("interpret cd") {
    val result   = interpret(List(TermOutput.Cd("/")))
    val expected = List(Command.Cd("/"))
    assertEquals(result, expected)
  }

  test("interpret ls") {
    val result   = interpret(List(TermOutput.Ls, TermOutput.Directory("hello"), TermOutput.File("ok", 2L)))
    val expected = List(Command.Ls(List(TermOutput.Directory("hello"), TermOutput.File("ok", 2L))))
    assertEquals(result, expected)
  }

  test("generate 1") {
    val result   = generate(List(Command.Cd("/")))
    val expected = mutable.Map[String, Directory]("/" -> Directory("/"))
    assertEquals(result, expected)
  }
