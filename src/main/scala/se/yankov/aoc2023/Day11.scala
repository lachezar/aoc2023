package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day11 extends IOApp.Simple {

  final case class Pos(y: Int, x: Int)

  final case class Space(y: Int, x: Int):
    def normalize: Space = this match
      case Space(0, 0) => Space.empty
      case other       => other

  object Space:
    def empty: Space  = Space(1, 1)
    def galaxy: Space = Space(0, 0)

  extension (field: Array[Array[Space]])
    def get(pos: Pos): Space = field(pos.y)(pos.x)

    def expandSpaceField(expansionRate: Int): Array[Array[Space]] =
      field
        .map((row: Array[Space]) => if row.forall(_.y == 1) then row.map(_.copy(y = expansionRate)) else row)
        .transpose
        .map((row: Array[Space]) => if row.forall(_.x == 1) then row.map(_.copy(x = expansionRate)) else row)
        .transpose

    def distance(first: Pos, second: Pos): Long =
      (if first.y > second.y then second.y to first.y else first.y to second.y)
        .map(Pos(_, first.x))
        .map(field.get(_).y)
        .sum - 1 +
        (if first.x > second.x then second.x to first.x else first.x to second.x)
          .map(Pos(second.y, _))
          .map(field.get(_).x)
          .sum - 1

  val task1: IO[Unit] = for {
    field: Array[Array[Char]]         <- Utils.readLines[IO]("day11.input.txt").map(_.toArray).compile.to(Array)
    galaxyPositions: Array[Pos]        = field
                                           .zipWithIndex
                                           .flatMap { (row, y) =>
                                             row.zipWithIndex.collect {
                                               case ('#', x) => Pos(y, x)
                                             }
                                           }
    expandedField: Array[Array[Space]] = field
                                           .map(_.map((c: Char) => if c == '.' then Space.empty else Space.galaxy))
                                           .expandSpaceField(2)
                                           .map(_.map(_.normalize))
    res: Long                          = galaxyPositions
                                           .combinations(2)
                                           .collect {
                                             case Array(first, second) => expandedField.distance(first, second)
                                           }
                                           .sum
    _                                 <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Array[Array[Char]]         <- Utils.readLines[IO]("day11.input.txt").map(_.toArray).compile.to(Array)
    galaxyPositions: Array[Pos]        = field
                                           .zipWithIndex
                                           .flatMap { (row, y) =>
                                             row.zipWithIndex.collect {
                                               case ('#', x) => Pos(y, x)
                                             }
                                           }
    expandedField: Array[Array[Space]] = field
                                           .map(_.map((c: Char) => if c == '.' then Space.empty else Space.galaxy))
                                           .expandSpaceField(1_000_000)
                                           .map(_.map(_.normalize))
    res: Long                          = galaxyPositions
                                           .combinations(2)
                                           .collect {
                                             case Array(first, second) => expandedField.distance(first, second)
                                           }
                                           .sum
    _                                 <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
