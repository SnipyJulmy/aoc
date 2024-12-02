package day02

class Day02Spec extends munit.FunSuite:
  test("dampened") {
    assert(isDampenerSafe(List(1, 2, 3, 4, 9)))
    assert(isDampenerSafe(List(65, 68, 71, 72, 71)))
    assert(isDampenerSafe(List()))
    assert(isDampenerSafe(List(1)))
    assert(isDampenerSafe(List(1, 4)))
    assert(isDampenerSafe(List(4, 1)))
    assert(isDampenerSafe(List(3, 9, 2)))
    assert(isDampenerSafe(List(11, 13, 8, 6, 5, 3, 2)))
    assert(isDampenerSafe(List(1, 3, 4, 5, 6, 7, 322)))
    assert(isDampenerSafe(List(3, 1, 3, 5)))

    assert(!isDampenerSafe(List(51, 54, 54, 57, 60, 60)))
    assert(!isDampenerSafe(List(43, 40, 41, 44, 47, 50, 51, 50)))
    assert(!isDampenerSafe(List(1, 2, 3, 4, 9, 9)))
    assert(!isDampenerSafe(List(2, 3, 5, 4, 4)))
    assert(!isDampenerSafe(List(3, 3, 4, 1, 5)))
    assert(!isDampenerSafe(List(85, 81, 78, 74, 70)))
    assert(!isDampenerSafe(List(4, 4, 4)))
    assert(!isDampenerSafe(List(4, 4, 4, 3)))
    assert(!isDampenerSafe(List(4, 4, 4, 2)))
    assert(!isDampenerSafe(List(4, 4, 4, 6)))
    assert(!isDampenerSafe(List(4, 4, 4, 8)))
  }
