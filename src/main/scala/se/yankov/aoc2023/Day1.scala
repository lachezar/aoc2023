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

  val task2: IO[Unit] = for {
    lines <- Utils.readLines[IO]("day1.input.txt").compile.toList
    res    = lines.map { l =>
               val foundFirst: String = regex.findFirstIn(l).get
               val first: Int         = words.getOrElse(foundFirst, foundFirst).toInt
               val foundLast: String  = reverseRegex.findFirstIn(l.reverse).get
               val last: Int          = words.map((k, v) => k.reverse -> v).getOrElse(foundLast, foundLast).toInt
               first * 10 + last
             }.sum
    _     <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
