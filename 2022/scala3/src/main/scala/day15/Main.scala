package day15

import scala.util.Using
import scala.util.control.NonLocalReturns.*

case class Point(x: Int, y: Int):
  lazy val tuningFrequency: Long      = x.toLong * 4000000L + y.toLong
  def manhattanDist(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs

case class Sensor(sensor: Point, beacon: Point, dist: Int):
  def contains(point: Point): Boolean = sensor.manhattanDist(point) <= dist

case class SensorMap(sensorsList: List[Sensor]):

  lazy val sensors: Set[Point] = sensorsList.map(_.sensor).toSet
  lazy val beacons: Set[Point] = sensorsList.map(_.beacon).toSet

  lazy val minX = (sensors.map(_.x) ++ beacons.map(_.x)).min
  lazy val maxX = (sensors.map(_.x) ++ beacons.map(_.x)).max
  lazy val minY = (sensors.map(_.y) ++ beacons.map(_.y)).min
  lazy val maxY = (sensors.map(_.y) ++ beacons.map(_.y)).max

  lazy val min = minX.min(minY) - (sensorsList.map(_.dist).max)
  lazy val max = maxX.min(maxY) + (sensorsList.map(_.dist).max)

  def show(): Unit =
    for (y <- min to max) {
      for (x <- min to max) {
        val pos = Point(x, y)
        if sensors.exists(_ == pos) then print("S")
        else if beacons.exists(b => b == pos) then print("B")
        else if sensorsList.exists(_.contains(pos)) then print("#")
        else print(".")
      }
      println()
    }

  def row(y: Int): Range = minX to maxX

  def countImpossibleBeacons(y: Int): Int =
    // find all possible sensors that are in range of y
    val reachableSensors = sensorsList.filter(s => s.sensor.manhattanDist(s.beacon) <= (y - s.sensor.y).abs)

    val min = minX.min(minY) - (reachableSensors.map(s => s.sensor.manhattanDist(s.beacon)).max)
    val max = maxX.max(maxY) + (reachableSensors.map(s => s.sensor.manhattanDist(s.beacon)).max)

    val ss = reachableSensors.map(_.sensor).toSet
    val bs = reachableSensors.map(_.beacon).toSet

    (for {
      x <- min to max
    } yield Point(x, y)).count(p =>
      !sensors.contains(p) &&
        !beacons.contains(p) &&
        sensorsList.exists(s => s.sensor.manhattanDist(s.beacon) >= s.sensor.manhattanDist(p))
    )

  def range(center: Point, radius: Int, y: Int): Range =
    val xDelta = radius - (center.y - y).abs
    if xDelta < 0 then 0 until 0
    else (center.x - xDelta) to (center.x + xDelta)

  def distressBeacons(min: Int, max: Int): Point =
    def inner(y: Int): Point =
      if y > max then throw new IllegalArgumentException("impossible")
      else
        merge(sensorsList.map(s => range(s.sensor, s.dist, y)))
          .find(range => min < range.start && range.start <= max + 1) match
          case None        => inner(y + 1)
          case Some(range) => Point(range.start - 1, y)
    inner(0)

def merge(rs: List[Range]): List[Range] =
  def inner(rs: List[Range], acc: List[Range] = Nil): List[Range] = rs match
    case x :: y :: xs =>
      if y.start > x.end then inner(y :: xs, x :: acc)
      else inner((x.start to (x.end.max(y.end))) :: xs, acc)
    case _ =>
      (rs ::: acc).reverse
  inner(rs.sortBy(_.start))

object Sensor:
  val regex = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r
  def fromString(string: String): Sensor = string match
    case regex(xs, ys, xb, yb) =>
      val sensor = Point(xs.toInt, ys.toInt)
      val beacon = Point(xb.toInt, yb.toInt)
      val dist   = sensor.manhattanDist(beacon)
      Sensor(sensor, beacon, dist)

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val sensorMap: SensorMap = SensorMap(input.map(Sensor.fromString))

  // sensorMap.show()
  // val score1 = sensorMap.countImpossibleBeacons(10)
  // val score2 = sensorMap.distressBeacons(0, 20).tuningFrequency

  val score1 = sensorMap.countImpossibleBeacons(2000000)
  val score2 = sensorMap.distressBeacons(0, 4000000).tuningFrequency

  println(s"======== Day 15 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
