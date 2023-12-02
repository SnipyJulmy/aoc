package day16

import scala.util.Using
import scala.collection.immutable._

extension [K, V](m: Map[K, V])
  def update(pred: (V, V) => Boolean)(key: K, newValue: V): Map[K, V] = m.get(key) match
    case None                                 => m + (key -> newValue)
    case Some(value) if pred(newValue, value) => m.updated(key, newValue)
    case Some(value)                          => m

case class Valve(name: String, flowRate: Int, tunnels: List[String])

def dijstra(valves: List[Valve])(source: Valve): Map[(Valve, Valve), List[Valve]] =

  val valvesMap: Map[String, Valve] = valves.map(v => v.name -> v).toMap

  def inner(jobs: Queue[(Valve, List[Valve])], visitedValves: Set[Valve], paths: Map[(Valve, Valve), List[Valve]]): Map[(Valve, Valve), List[Valve]] =
    if jobs.isEmpty then paths
    else
      val ((crtValve, crtPaths), rest) = jobs.dequeue
      val neighbours                   = crtValve.tunnels.map(valvesMap.apply).filterNot(visitedValves.contains)
      inner(rest ++ neighbours.map(v => v -> (crtValve :: crtPaths)), visitedValves + crtValve, paths + ((source -> crtValve) -> (crtValve :: crtPaths)))

  inner(Queue(source -> Nil), Set(), Map((source -> source) -> Nil)).mapValues(_.reverse).toMap

def maxPressure(valves: List[Valve], totalTime: Int): Int =
  val valvesMap: Map[String, Valve]           = valves.map(v => v.name -> v).toMap
  val paths: Map[(Valve, Valve), List[Valve]] = valves.map(v => dijstra(valves)(v)).reduce(_ ++ _)
  val start                                   = valves.head

  def bfs(jobs: Queue[(Int, Valve, Set[Valve], Int)], scores: Map[Int, Int]): Int =
    if jobs.isEmpty then 0
    else
      val ((remainingTime, currentValve, openValves, releasedPressure), rest) = jobs.dequeue
      println(scores)
      scala.io.StdIn.readLine()
      if remainingTime == 0 then releasedPressure + openValves.map(_.flowRate).sum
      else if openValves(currentValve) then
        val score = releasedPressure + openValves.map(_.flowRate).sum
        bfs(
          rest.enqueueAll(
            currentValve.tunnels
              .map(valvesMap.apply)
              .map(v => (remainingTime - 1, v, openValves, score))
          ),
          scores.update(_ > _)(remainingTime, score)
        )
      else
        val score = releasedPressure + openValves.map(_.flowRate).sum
        bfs(
          rest.enqueueAll(
            (remainingTime - 1, currentValve, openValves + currentValve, score) ::
              currentValve.tunnels
                .map(valvesMap.apply)
                .map(v => (remainingTime - 1, v, openValves, score))
          ),
          scores.update(_ > _)(remainingTime, score)
        )

  bfs(Queue((totalTime, start, Set(), 0)), Map())

object Valve:
  def parse(string: String): Valve =
    val regex = raw"Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z][A-Z](?:, [A-Z][A-Z])*)".r
    string match {
      case regex(name, rate, valves) => Valve(name, rate.toInt, valves.split(",").map(_.trim()).toList)
    }

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val valves: List[Valve] = input.map(Valve.parse)

  val score1 = maxPressure(valves, 30)
  val score2 = 0

  println(s"======== Day 16 ========")
  println(s"Score part 1  : $score1")
  println(s"Score part 2  : $score2")
  println("=" * 24)
