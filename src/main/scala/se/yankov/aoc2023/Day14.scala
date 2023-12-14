package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec
import util.chaining.scalaUtilChainingOps

object Day14 extends IOApp.Simple {

  extension (field: Vector[String])
    def render: String = field.mkString("\n")
    def totalLoad: Int = field.transpose.map(_.reverse.zipWithIndex.collect { case 'O' -> i => i + 1 }.sum).sum

  def tilt(
      field: Vector[String],
      yTransformation: (Vector[String]) => Vector[String],
      xTransformation: (String) => String,
    ): Vector[String] =
    yTransformation(field).map(xTransformation(_).split("#", -1).map(_.sortBy(-_)).mkString("#")) pipe
      ((transformedField: Vector[String]) => yTransformation(transformedField.map(xTransformation(_))))

  def tiltNorth(field: Vector[String]): Vector[String] = tilt(field, _.transpose.map(_.mkString), identity)

  def tiltWest(field: Vector[String]): Vector[String] = tilt(field, identity, identity)

  def tiltSouth(field: Vector[String]): Vector[String] = tilt(field, _.transpose.map(_.mkString), _.reverse)

  def tiltEast(field: Vector[String]): Vector[String] = tilt(field, identity, _.reverse)

  val cycle: Vector[String] => Vector[String] = tiltEast compose tiltSouth compose tiltWest compose tiltNorth

  @tailrec
  def computeBillionthCycle(field: Vector[String], previousCycles: Vector[Vector[String]]): Vector[String] =
    val res: Vector[String]     = cycle(field)
    val matchingCycleIndex: Int = previousCycles.indexOf(res)
    if matchingCycleIndex > -1 then {
      val cyclePeriod: Int         = previousCycles.length - matchingCycleIndex
      val billionthCycleIndex: Int = (1_000_000_000 - matchingCycleIndex) % cyclePeriod + matchingCycleIndex - 1
      previousCycles(billionthCycleIndex)
    }
    else computeBillionthCycle(res, previousCycles :+ res)

  val task1: IO[Unit] = for {
    field: Vector[String] <- Utils.readLines[IO]("day14.input.txt").compile.toVector
    res: Int               = tiltNorth(field).totalLoad
    _                     <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Vector[String] <- Utils.readLines[IO]("day14.input.txt").compile.toVector
    res: Int               = computeBillionthCycle(field, Vector.empty).totalLoad
    _                     <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
