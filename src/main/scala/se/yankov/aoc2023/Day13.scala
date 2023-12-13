package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day13 extends IOApp.Simple {

  extension (v: Vector[Char]) def refraction(other: Vector[Char]): Int = v.zip(other).filterNot(_ == _).length

  def horizontalMirror(field: Vector[String], refractionIndex: Int): Option[Int] =
    (1 until field.length).find { (point: Int) =>
      val (part1: Vector[String], part2: Vector[String]) = field.splitAt(point)
      part1.transpose.map(_.reverse).zip(part2.transpose).foldLeft(0) {
        case (acc: Int, (columnPart1: Vector[Char], columnPart2: Vector[Char])) =>
          val commonLength: Int = Math.min(columnPart1.length, columnPart2.length)
          acc + columnPart1.take(commonLength).refraction(columnPart2.take(commonLength))
      } == refractionIndex
    }

  def verticalMirror(field: Vector[String], refractionIndex: Int): Option[Int] =
    (1 until field(0).length).find { (point: Int) =>
      field.map(_.toVector.splitAt(point)).foldLeft(0) {
        case (acc: Int, (rowPart1: Vector[Char], rowPart2: Vector[Char])) =>
          val commonLength: Int = Math.min(rowPart1.length, rowPart2.length)
          acc + rowPart1.reverse.take(commonLength).refraction(rowPart2.take(commonLength))
      } == refractionIndex
    }

  val task1: IO[Unit] = for {
    lines: Vector[String]         <- Utils.readLines[IO]("day13.input.txt").compile.toVector
    fields: Vector[Vector[String]] = lines.foldLeft(Vector(Vector.empty[String])) {
                                       case acc -> ""        => acc :+ Vector.empty
                                       case (t :+ h) -> line => t :+ (h :+ line)
                                       case acc -> _         => acc
                                     }
    res: Int                       = fields.map(horizontalMirror(_, 0)).map(_.getOrElse(0) * 100).sum +
                                       fields.map(verticalMirror(_, 0)).map(_.getOrElse(0)).sum
    _                             <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: Vector[String]         <- Utils.readLines[IO]("day13.input.txt").compile.toVector
    fields: Vector[Vector[String]] = lines.foldLeft(Vector(Vector.empty[String])) {
                                       case acc -> ""        => acc :+ Vector.empty
                                       case (t :+ h) -> line => t :+ (h :+ line)
                                       case acc -> _         => acc
                                     }
    res: Int                       = fields.map(horizontalMirror(_, 1)).map(_.getOrElse(0) * 100).sum +
                                       fields.map(verticalMirror(_, 1)).map(_.getOrElse(0)).sum
    _                             <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
