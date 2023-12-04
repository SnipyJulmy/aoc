package day02

import aoc.ParseException

class Day02Spec extends munit.FunSuite:

  val parser = new GameParser

  checkParseGame("Game 2: 3 red", Game(2, List(Reveal(3, 0, 0))))
  checkParseGame("Game 2: 3 green", Game(2, List(Reveal(0, 3, 0))))
  checkParseGame("Game 2: 3 blue", Game(2, List(Reveal(0, 0, 3))))

  checkParseGame("Game 18: 3 red, 2 blue, 4 green;3 red", Game(18, List(Reveal(3, 4, 2), Reveal(3, 0, 0))))

  checkParseGame(
    "Game 1: 1 green, 6 red, 4 blue; 2 blue, 6 green, 7 red; 3 red, 4 blue, 6 green; 3 green; 3 blue, 2 green, 1 red",
    Game(
      1,
      List(
        Reveal(6, 1, 4),
        Reveal(7, 6, 2),
        Reveal(3, 6, 4),
        Reveal(0, 3, 0),
        Reveal(1, 2, 3),
      )
    )
  )

  checkParseGameFails("Game 2 3 blue")

  @inline
  def checkParseGame(input: String, expected: Game) =
    test(s"check parseGame($input)") {
      assertEquals(parser.parseGame(input), expected)
    }

  @inline
  def checkParseGameFails(input: String) =
    test(s"check parseGame($input) fails") {
      intercept[ParseException] {
        parser.parseGame(input)
      }
    }
