package day12

class Day12Spec extends munit.FunSuite:
  test("bfs length 0") {
    val map = Vector(
      Vector(1, 1, 1, 1),
      Vector(2, 2, 2, 2),
      Vector(3, 4, 4, 4),
      Vector(3, 4, 4, 5)
    )
    assertEquals(bfs(map)((1, 2), (1, 2)), Some(0))
  }

  test("bfs length 1") {
    val map = Vector(
      Vector(1, 1, 1, 1),
      Vector(2, 2, 2, 2),
      Vector(3, 4, 4, 4),
      Vector(3, 4, 4, 5)
    )
    assertEquals(bfs(map)((0, 0), (0, 1)), Some(1))
    assertEquals(bfs(map)((0, 0), (1, 0)), Some(1))
  }


  test("bfs length 6") {
    val map = Vector(
      Vector(1, 1, 1, 1),
      Vector(2, 2, 2, 2),
      Vector(3, 4, 4, 4),
      Vector(3, 4, 4, 5)
    )
    assertEquals(bfs(map)((0, 0), (3, 3)), Some(6))
  }

  test("bfs impossible") {
    val map = Vector(
      Vector(1, 1, 1, 1),
      Vector(2, 2, 2, 2),
      Vector(3, 4, 4, 4),
      Vector(3, 4, 4, 7)
    )
    assertEquals(bfs(map)((0, 0), (3, 3)), None)
  }


