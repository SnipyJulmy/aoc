val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "2023",
    scalaVersion := scala3Version
  )

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

commands ++= (1 to 25).toSeq.map { day =>
  Command.command(f"day$day%02d") { state => runDay(day) :: state }
}

commands ++= (1 to 25).toSeq.map { day =>
  Command.command(f"day$day%02d-example") { state => runDayExample(day) :: state }
}

commands ++= (1 to 25).toSeq.map { day =>
  Command.command(f"day$day%02d-example2") { state => runDayExample2(day) :: state }
}

def runDay(day: Int): String        = f"runMain day$day%02d.main ../inputs/day$day%02d.txt"
def runDayExample(day: Int): String = f"runMain day$day%02d.main ../inputs/day$day%02d-example.txt"
def runDayExample2(day: Int): String = f"runMain day$day%02d.main ../inputs/day$day%02d-example2.txt"
