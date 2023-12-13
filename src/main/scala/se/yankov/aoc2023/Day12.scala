package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.util.matching.Regex

object Day12 extends IOApp.Simple {

  val multipleDots: Regex = "\\.+".r

  def parseLine(line: String): IO[(String, Array[Int])] =
    line.split(' ') match
      case Array(template, groups) => IO(multipleDots.replaceAllIn(template, ".") -> groups.split(',').map(_.toInt))
      case _                       => IO.raiseError(new RuntimeException("invalid input"))

  def consume(tape: Vector[Char], groups: List[Int]): Long =
    val memoization: collection.mutable.Map[(Vector[Char], List[Int]), Long] = collection.mutable.Map.empty

    def consume_(tape: Vector[Char], groups: List[Int]): Long =
      memoization.get(tape -> groups) match
        case Some(value) => value
        case None        =>
          val result: Long = if (groups.isEmpty) {
            if tape.contains('#') then 0 else 1
          }
          else if (tape.length < groups.head) 0
          else if (tape.take(groups.head).contains('.')) {
            val dotIndex: Int = tape.indexOf('.')
            if tape.take(dotIndex + 1).contains('#') then 0
            else consume_(tape.drop(dotIndex + 1), groups)
          }
          else {
            (if tape.length == groups.head || List('.', '?').contains(tape(groups.head))
             then consume_(tape.drop(groups.head + 1), groups.tail)
             else 0) +
              (if tape.head == '#' then 0 else consume_(tape.tail, groups))
          }
          memoization.update(tape -> groups, result)
          result

    consume_(tape, groups)

  val task1: IO[Unit] = Utils
    .readLines[IO]("day12.input.txt")
    .evalMap(parseLine)
    .map((template: String, groups: Array[Int]) => consume(template.toVector, groups.toList))
    .fold(0L)(_ + _)
    .printlns
    .compile
    .drain

  val task2: IO[Unit] = Utils
    .readLines[IO]("day12.input.txt")
    .evalMap(parseLine)
    .map { (template: String, groups: Array[Int]) =>
      consume(Vector.fill(5)(template).mkString("?").toVector, List.fill(5)(groups).flatten).toLong
    }
    .fold(0L)(_ + _)
    .printlns
    .compile
    .drain

  def run: IO[Unit] = task1 >> task2
}
