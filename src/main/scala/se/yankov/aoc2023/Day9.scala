package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.annotation.tailrec

object Day9 extends IOApp.Simple {

  @tailrec
  def findNext(sequence: Array[Int], aggregator: Int): Int =
    if sequence.forall(_ == 0) then aggregator
    else
      val newSequence: Array[Int] = sequence.sliding(2).map((pair: Array[Int]) => pair(1) - pair(0)).toArray
      findNext(newSequence, aggregator + sequence.last)

  @tailrec
  def findPrev(sequence: Array[Int], headElements: List[Int]): List[Int] =
    if sequence.forall(_ == 0) then headElements
    else
      val newSequence: Array[Int] = sequence.sliding(2).map((pair: Array[Int]) => pair(1) - pair(0)).toArray
      findPrev(newSequence, sequence.head :: headElements)

  val task1: IO[Unit] = for {
    lines: List[Array[Int]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toInt)).compile.toList
    res: Int                 = lines.map(findNext(_, 0)).sum
    _                       <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[Array[Int]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toInt)).compile.toList
    res: Int                 = lines.map(findPrev(_, Nil).fold(0)((a: Int, b: Int) => b - a)).sum
    _                       <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
