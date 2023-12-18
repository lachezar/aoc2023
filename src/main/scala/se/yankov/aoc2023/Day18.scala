package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day18 extends IOApp.Simple {

  enum Direction(val y: Int, val x: Int):
    case Left  extends Direction(0, -1)
    case Right extends Direction(0, 1)
    case Up    extends Direction(-1, 0)
    case Down  extends Direction(1, 0)

  final case class Pos(y: Int, x: Int):
    def move(direction: Direction, steps: Int): Pos = Pos(y + direction.y * steps, x + direction.x * steps)

  final case class PlanPart(direction: Direction, steps: Int)

  // The idea is detailed at https://en.wikipedia.org/wiki/Shoelace_formula
  def area(plan: List[PlanPart]): Long =
    val innerArea: Long = plan
      .foldLeft(Pos(0, 0) :: Nil) {
        case (Nil, _)                                        => Nil
        case ((pos: Pos) :: (rest: List[Pos]), pp: PlanPart) => pos.move(pp.direction, pp.steps) :: pos :: rest
      }
      .sliding(2)
      .foldLeft(0L) {
        case (acc: Long, (p1: Pos) :: (p2: Pos) :: _) => acc + p1.y.toLong * p2.x.toLong - p1.x.toLong * p2.y.toLong
        case (acc: Long, _)                           => acc
      } / 2
    val perimeter: Int  = plan.map(_.steps).sum
    innerArea + perimeter / 2 + 1

  val task1: IO[Unit] = for {
    plan: List[PlanPart] <- Utils
                              .readLines[IO]("day18.input.txt")
                              .collect {
                                case s"L $steps ($_)" => PlanPart(Direction.Left, steps.toInt)
                                case s"R $steps ($_)" => PlanPart(Direction.Right, steps.toInt)
                                case s"U $steps ($_)" => PlanPart(Direction.Up, steps.toInt)
                                case s"D $steps ($_)" => PlanPart(Direction.Down, steps.toInt)
                              }
                              .compile
                              .toList
    res: Long             = area(plan)
    _                    <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    plan: List[PlanPart] <-
      Utils
        .readLines[IO]("day18.input.txt")
        .collect {
          case s"$_ $_ (#${hexSteps}2)" => PlanPart(Direction.Left, Integer.parseInt(hexSteps, 16))
          case s"$_ $_ (#${hexSteps}0)" => PlanPart(Direction.Right, Integer.parseInt(hexSteps, 16))
          case s"$_ $_ (#${hexSteps}3)" => PlanPart(Direction.Up, Integer.parseInt(hexSteps, 16))
          case s"$_ $_ (#${hexSteps}1)" => PlanPart(Direction.Down, Integer.parseInt(hexSteps, 16))
        }
        .compile
        .toList
    res: Long             = area(plan)
    _                    <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
