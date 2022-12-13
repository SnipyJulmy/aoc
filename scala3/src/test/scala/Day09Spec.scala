package day09

class Day09Spec extends munit.FunSuite:
  test("fix") {
    assertEquals(fix(Pos(0, 0))(Pos(-1, -1)), Pos(-1, -1))
    assertEquals(fix(Pos(0, 0))(Pos(-1, 0)), Pos(-1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(-1, 1)), Pos(-1, 1))
    assertEquals(fix(Pos(0, 0))(Pos(0, -1)), Pos(0, -1))
    assertEquals(fix(Pos(0, 0))(Pos(0, 0)), Pos(0, 0))
    assertEquals(fix(Pos(0, 0))(Pos(0, 1)), Pos(0, 1))
    assertEquals(fix(Pos(0, 0))(Pos(1, -1)), Pos(1, -1))
    assertEquals(fix(Pos(0, 0))(Pos(1, 0)), Pos(1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(1, 1)), Pos(1, 1))
    assertEquals(fix(Pos(0, 0))(Pos(2, -2)), Pos(1, -1))
    assertEquals(fix(Pos(0, 0))(Pos(2, -1)), Pos(1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(2, 0)), Pos(1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(2, 1)), Pos(1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(2, 2)), Pos(1, 1))
    assertEquals(fix(Pos(0, 0))(Pos(1, -2)), Pos(0, -1))
    assertEquals(fix(Pos(0, 0))(Pos(1, 2)), Pos(0, 1))
    assertEquals(fix(Pos(0, 0))(Pos(0, -2)), Pos(0, -1))
    assertEquals(fix(Pos(0, 0))(Pos(0, 2)), Pos(0, 1))
    assertEquals(fix(Pos(0, 0))(Pos(-1, -2)), Pos(0, -1))
    assertEquals(fix(Pos(0, 0))(Pos(-1, 2)), Pos(0, 1))
    assertEquals(fix(Pos(0, 0))(Pos(-2, -2)), Pos(-1, -1))
    assertEquals(fix(Pos(0, 0))(Pos(-2, -1)), Pos(-1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(-2, 0)), Pos(-1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(-2, 1)), Pos(-1, 0))
    assertEquals(fix(Pos(0, 0))(Pos(-2, 2)), Pos(-1, 1))
  }

  test("arrange") {
    assertEquals(arrange(Rope.of(10)), Rope.of(10))
  }

  test("simulate 1") {
    import Movement._
    val movements = List(Up, Down, Up, Down, Right, Right, Right, Right, Up, Up, Up, Right, Up)
    val a         = simulate(movements)
    val b         = simulate(movements, 2)
    assertEquals(a._2, b._2)
  }

  test("simulate") {
    import Movement._

    for (i <- 0 until 10) {
      val movements = List.fill(scala.util.Random.nextInt(10))(scala.util.Random.shuffle(List(Down, Up, Left, Right)).head)
      val a = simulate(movements)
      val b = simulate(movements, 2)
      assertEquals(a._2, b._2)
    }
  }
