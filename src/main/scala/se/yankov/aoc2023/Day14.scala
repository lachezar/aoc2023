package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec

object Day14 extends IOApp.Simple {

  final case class NormalizedRow(offset: Int, amount: Int):
    def range: Range                   = offset until (offset + amount)
    def incrementAmount: NormalizedRow = copy(amount = amount + 1)

  extension (field: Vector[Vector[Char]])
    def render: String = field.map(_.mkString).mkString("\n")
    def totalLoad: Int = field
      .transpose
      .map(_.reverse.zipWithIndex.collect { case 'O' -> i => i + 1 }.sum)
      .sum

  def tilt(
      field: Vector[Vector[Char]],
      yTransformation: (Vector[Vector[Char]]) => Vector[Vector[Char]],
      xTransformation: (Vector[Char]) => Vector[Char],
    ): Vector[Vector[Char]] =
    val normalizedField: Vector[Vector[NormalizedRow]] = yTransformation(field).map { (row: Vector[Char]) =>
      xTransformation(row).zipWithIndex.foldLeft(Vector(NormalizedRow(0, 0))) {
        case (acc: Vector[NormalizedRow], (c: Char, i: Int)) =>
          c match
            case '#' => acc :+ NormalizedRow(i + 1, 0)
            case 'O' => acc.dropRight(1) :+ acc.last.incrementAmount
            case _   => acc
      }
    }
    val cleanLine: Vector[Char]                        = Vector.fill(field.length)('.')
    yTransformation(normalizedField.map { (row: Vector[NormalizedRow]) =>
      xTransformation(row.foldLeft(cleanLine) {
        case (line: Vector[Char], nr @ NormalizedRow(offset, amount)) =>
          // Restore the normalized matrix back to the original format
          val lineWithSquareStone: Vector[Char] = if offset > 0 then line.updated(offset - 1, '#') else line
          lineWithSquareStone.patch(offset, Vector.fill(amount)('O'), amount)
      })
    })

  def tiltNorth(field: Vector[Vector[Char]]): Vector[Vector[Char]] = tilt(field, _.transpose, identity)

  def tiltWest(field: Vector[Vector[Char]]): Vector[Vector[Char]] = tilt(field, identity, identity)

  def tiltSouth(field: Vector[Vector[Char]]): Vector[Vector[Char]] = tilt(field, _.transpose, _.reverse)

  def tiltEast(field: Vector[Vector[Char]]): Vector[Vector[Char]] = tilt(field, identity, _.reverse)

  val cycle: Vector[Vector[Char]] => Vector[Vector[Char]] =
    tiltEast compose tiltSouth compose tiltWest compose tiltNorth

  @tailrec
  def findBillionthCycle(field: Vector[Vector[Char]], previousCycles: Vector[Vector[Vector[Char]]])
      : Vector[Vector[Char]] =
    val res: Vector[Vector[Char]] = cycle(field)
    val matchingCycleIndex: Int   = previousCycles.indexOf(res)
    if matchingCycleIndex > -1 then {
      val cyclePeriod: Int        = previousCycles.length - matchingCycleIndex
      val billionCyclesIndex: Int = (1_000_000_000 - matchingCycleIndex) % cyclePeriod + matchingCycleIndex - 1
      previousCycles(billionCyclesIndex)
    }
    else findBillionthCycle(res, previousCycles :+ res)

  val task1: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day14.input.txt").map(_.toVector).compile.toVector
    res: Int                     = tiltNorth(field).totalLoad
    _                           <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day14.input.txt").map(_.toVector).compile.toVector
    res: Int                     = findBillionthCycle(field, Vector.empty).totalLoad
    _                           <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
