val scala3Version = "3.2.1"

val runAOC = taskKey[Unit]("run all aoc problems")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "aoc",
    version      := "2022",
    scalaVersion := scala3Version
  )

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

commands ++= (1 to 25).toSeq.map { day =>
  Command.command(f"day$day%02d") { state => runDay(day) :: state }
}

def runDay(day: Int): String = f"runMain day$day%02d.main ../day$day%02d.txt"
