package day05

class Day02Spec extends munit.FunSuite:
  test("fix with 2 elements") {
    val f = fix(List(1 -> 2, 2 -> 3))
    assertEquals(f(List(1, 2)), List(1, 2))
    assertEquals(f(List(2, 1)), List(1, 2))
    assertEquals(f(List(1, 3)), List(1, 3))
    assertEquals(f(List(3, 1)), List(3, 1))
    assertEquals(f(List(2, 3)), List(2, 3))
    assertEquals(f(List(3, 2)), List(2, 3))
  }

  test("fix with 3 elements") {
    val f = fix(List(1 -> 2, 2 -> 3))
    assertEquals(f(List(1, 2, 3)), List(1, 2, 3))
    assertEquals(f(List(1, 3, 2)), List(1, 2, 3))
    assertEquals(f(List(2, 1, 3)), List(1, 2, 3))
    assertEquals(f(List(2, 3, 1)), List(1, 2, 3))
    assertEquals(f(List(3, 2, 1)), List(1, 2, 3))
    assertEquals(f(List(3, 1, 2)), List(1, 2, 3))
  }
