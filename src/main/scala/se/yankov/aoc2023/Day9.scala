package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec

object Day9 extends IOApp.Simple {

  def findNext(sequence: Array[Long]): Long = findElement(sequence, (_.last), Nil).sum

  def findPrev(sequence: Array[Long]): Long = findElement(sequence, (_.head), Nil).reduce((a: Long, b: Long) => b - a)

  @tailrec
  def findElement(sequence: Array[Long], getter: (Array[Long] => Long), edgeElements: List[Long]): List[Long] =
    if sequence.forall(_ == 0) then edgeElements
    else
      val newSequence: Array[Long] = sequence.sliding(2).map((pair: Array[Long]) => pair(1) - pair(0)).toArray
      findElement(newSequence, getter, getter(sequence) :: edgeElements)

  val task1: IO[Unit] = for {
    lines: List[Array[Long]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toLong)).compile.toList
    res: Long                 = lines.map(findNext).sum
    _                        <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[Array[Long]] <- Utils.readLines[IO]("day9.input.txt").map(_.split(' ').map(_.toLong)).compile.toList
    res: Long                 = lines.map(findPrev).sum
    _                        <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
