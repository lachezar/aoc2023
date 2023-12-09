package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.annotation.tailrec

object Day9 extends IOApp.Simple {

  def findNext(sequence: Array[Int]): Int = findElement(sequence, (_.last), Nil).sum

  def findPrev(sequence: Array[Int]): Int = findElement(sequence, (_.head), Nil).reduce((a: Int, b: Int) => b - a)

  @tailrec
  def findElement(sequence: Array[Int], getter: (Array[Int] => Int), edgeElements: List[Int]): List[Int] =
    if sequence.forall(_ == 0) then edgeElements
    else
      val newSequence: Array[Int] = sequence.sliding(2).map((pair: Array[Int]) => pair(1) - pair(0)).toArray
      findElement(newSequence, getter, getter(sequence) :: edgeElements)

  val task1: IO[Unit] = for {
    lines: List[Array[Int]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toInt)).compile.toList
    res: Int                 = lines.map(findNext).sum
    _                       <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[Array[Int]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toInt)).compile.toList
    res: Int                 = lines.map(findPrev).sum
    _                       <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
