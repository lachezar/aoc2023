package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.util.matching.Regex

object Day1 extends IOApp.Simple {

  val task1: IO[Unit] = for {
    lines <- Utils.readLines[IO]("day1.input.txt").compile.toList.dbg
    res    = lines.map { l =>
               val digits: String = l.filter(_.isDigit)
               (digits.take(1) + digits.takeRight(1)).toInt
             }.sum
    _     <- IO.println(res)
  } yield ()

  val words: Map[String, String] =
    Map(
      "one"   -> "1",
      "two"   -> "2",
      "three" -> "3",
      "four"  -> "4",
      "five"  -> "5",
      "six"   -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine"  -> "9",
    )
  val regex: Regex               = s"""\\d|${words.keySet.mkString("|")}""".r
  val reverseRegex: Regex        = s"""\\d|${words.keySet.map(_.reverse).mkString("|")}""".r

  val task2: IO[Unit] =
    Utils
      .readLines[IO]("day1.input.txt")
      .evalMap { l =>
        for {
          foundFirst: String <-
            IO.fromOption(regex.findFirstIn(l))(new RuntimeException("invalid input"))
          first: Int          = words.getOrElse(foundFirst, foundFirst).toInt
          foundLast: String  <-
            IO.fromOption(reverseRegex.findFirstIn(l.reverse))(
              new RuntimeException("invalid input")
            )
          last: Int           =
            words.map((k, v) => k.reverse -> v).getOrElse(foundLast, foundLast).toInt
        } yield first * 10 + last
      }
      .fold(0)(_ + _)
      .evalTap(IO.println)
      .compile
      .drain

  def run: IO[Unit] = task2
}
