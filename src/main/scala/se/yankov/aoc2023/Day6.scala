package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day6 extends IOApp.Simple {

  val epsilon: Double = 0.000000001

  def solveQuadratic(a: Long, b: Long, c: Long): (Double, Double) =
    ((-b + Math.sqrt(b.toDouble * b - 4 * a * c)) / 2 * a) -> ((-b - Math.sqrt(b.toDouble * b - 4 * a * c)) / 2 * a)

  val task1: IO[Unit] = for {
    lines: Vector[String] <- Utils.readLines[IO]("day6.input.txt").compile.toVector
    times: List[Int]       = lines(0).split("\\s+").drop(1).map(_.toInt).toList
    distances: List[Int]   = lines(1).split("\\s+").drop(1).map(_.toInt).toList
    _                     <- IO.println(
                               times
                                 .zip(distances)
                                 .map((x, y) => solveQuadratic(-1, x, -y))
                                 .map((s1, s2) => (s2 - epsilon).floor.toInt - (s1 + epsilon).ceil.toInt + 1)
                                 .product
                             )
  } yield ()

  val task2: IO[Unit] = for {
    lines: Vector[String]   <- Utils.readLines[IO]("day6.input.txt").compile.toVector
    time: Long               = lines(0).split("\\s+").drop(1).mkString.toLong
    distance: Long           = lines(1).split("\\s+").drop(1).mkString.toLong
    (s1: Double, s2: Double) = solveQuadratic(-1, time, -distance)
    _                       <- IO.println((s2 - epsilon).floor.toInt - (s1 + epsilon).ceil.toInt + 1)
  } yield ()

  def run: IO[Unit] = task2
}
