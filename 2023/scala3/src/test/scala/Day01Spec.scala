package day01

class Day01Spec extends munit.FunSuite:
  (1 to 9).foreach { digit =>
    test(s"test toInt $digit") {
      assertEquals(toInt(digit.toString), digit.toString)
    }
  }

  test("test toInt one") {
    assertEquals(toInt("one"), "1")
  }

  test("test toInt two") {
    assertEquals(toInt("two"), "2")
  }

  test("test toInt three") {
    assertEquals(toInt("three"), "3")
  }

  test("test toInt four") {
    assertEquals(toInt("four"), "4")
  }

  test("test toInt five") {
    assertEquals(toInt("five"), "5")
  }

  test("test toInt six") {
    assertEquals(toInt("six"), "6")
  }

  test("test toInt seven") {
    assertEquals(toInt("seven"), "7")
  }

  test("test toInt eight") {
    assertEquals(toInt("eight"), "8")
  }

  test("test toInt nine") {
    assertEquals(toInt("nine"), "9")
  }

  checkCalibrationWithLetters("1", 11)
  checkCalibrationWithLetters("12", 12)
  checkCalibrationWithLetters("one", 11)
  checkCalibrationWithLetters("two", 22)
  checkCalibrationWithLetters("oneight", 18)
  checkCalibrationWithLetters("eightthreeg9nine7six", 86)

  @inline
  def checkCalibrationWithLetters(input: String, expected: Int) =
    test(s"calibrationWithLetters $input") {
      assertEquals(calibrationWithLetters(input), expected)
    }
