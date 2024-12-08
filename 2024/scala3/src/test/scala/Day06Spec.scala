package day06

class Day06Spec extends munit.FunSuite:
  test("isLooping no obstacle") {
    val map = Vector(
      Vector('.', '.', '.'),
      Vector('.', '.', '.'),
      Vector('.', '.', '.')
    )
    assert(!isLooping(map, 0, 0, Direction.N))
    assert(!isLooping(map, 0, 0, Direction.S))
    assert(!isLooping(map, 0, 0, Direction.W))
    assert(!isLooping(map, 0, 0, Direction.E))
  }

  test("isLooping 4x4 cross") {
    val map = Vector(
      Vector('.', '#', '.', '.'),
      Vector('.', '.', '.', '#'),
      Vector('#', '.', '.', '.'),
      Vector('.', '.', '#', '.')
    )
    assert(isLooping(map, 1, 1, Direction.N))
    assert(isLooping(map, 1, 1, Direction.E))
    assert(!isLooping(map, 1, 1, Direction.S))
    assert(!isLooping(map, 1, 1, Direction.W))
  }

  test("isLooping 3x3") {
    val map = Vector(
      Vector('#', '#', '#'),
      Vector('#', '.', '#'),
      Vector('#', '#', '#')
    )
    assert(isLooping(map, 1, 1, Direction.N))
    assert(isLooping(map, 1, 1, Direction.S))
    assert(isLooping(map, 1, 1, Direction.W))
    assert(isLooping(map, 1, 1, Direction.E))
  }

  test("isLooping 1") {
    val map = Vector(
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
    )
    assert(isLooping(map, 2, 3, Direction.N))
  }

  test("isLooping 1") {
    val map = Vector(
      Vector('.', '.', '#', '.', '.', '#', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '^', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
    )
    assert(isLooping(map, 5, 5, Direction.N))
    assert(isLooping(map, 6, 5, Direction.N))
    assert(isLooping(map, 7, 5, Direction.N))
    assert(isLooping(map, 8, 5, Direction.N))
  }

  test("Simulate") {
    val map = Vector(
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '^', '.', '.', '.', '.', '.', '#', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
    )
    val (visited, possibleObstacle) = simulate(map)
    assertEquals(visited, 20)
    assertEquals(possibleObstacle, 1)
  }

  test("Simulate empty") {
    val map = Vector(
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '^', '.', '.', '.', '.', '.'),
      Vector('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
    )
    val (visited, possibleObstacle) = simulate(map)
    assertEquals(visited, 10)
    assertEquals(possibleObstacle, 0)
  }
