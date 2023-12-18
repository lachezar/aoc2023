package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

object Day17 extends IOApp.Simple {

  enum Direction:
    case Left, Right, Up, Down

  final case class Pos(y: Int, x: Int)

  final case class Move(pos: Pos, direction: Direction, sameDirection: Int)

  extension [A](field: Vector[Vector[A]])
    def get(pos: Pos): A             = field(pos.y)(pos.x)
    def isInvalid(pos: Pos): Boolean = pos.y < 0 || pos.y >= field.length || pos.x < 0 || pos.x >= field(0).length
    def render: String               = field.map(_.mkString).mkString("\n")
    def up(pos: Pos): Option[Pos]    = Option.when(!isInvalid(pos.copy(y = pos.y - 1)))(pos.copy(y = pos.y - 1))
    def down(pos: Pos): Option[Pos]  = Option.when(!isInvalid(pos.copy(y = pos.y + 1)))(pos.copy(y = pos.y + 1))
    def left(pos: Pos): Option[Pos]  = Option.when(!isInvalid(pos.copy(x = pos.x - 1)))(pos.copy(x = pos.x - 1))
    def right(pos: Pos): Option[Pos] = Option.when(!isInvalid(pos.copy(x = pos.x + 1)))(pos.copy(x = pos.x + 1))

  def heatLoss(field: Vector[Vector[Int]], minSteps: Int, maxSteps: Int): Int =
    val pq: PriorityQueue[(Move, Int)] = PriorityQueue(
      Move(Pos(0, 0), Direction.Down, 1)  -> 0,
      Move(Pos(0, 0), Direction.Right, 1) -> 0,
    )(Ordering.by((_, cost) => -cost))

    @tailrec
    def heatLoss_(visited: Set[Move]): Int =
      val (move: Move, cost: Int) = pq.dequeue()

      if move.pos == Pos(field.length - 1, field(0).length - 1) && move.sameDirection >= minSteps then cost
      else {
        if (!visited.contains(move)) {
          val newMoves: List[(Move, Int)] =
            (field.up(move.pos).flatMap { (up: Pos) =>
              Option.when(
                move.direction != Direction.Down && (move.direction != Direction.Up || move.sameDirection < maxSteps) &&
                (move.direction == Direction.Up || move.sameDirection >= minSteps)
              ) {
                val directionSteps: Int = if move.direction == Direction.Up then move.sameDirection + 1 else 1
                Move(up, Direction.Up, directionSteps) -> (cost + field.get(up))
              }
            } ::
              field.down(move.pos).flatMap { (down: Pos) =>
                Option.when(
                  move.direction != Direction.Up && (move.direction != Direction.Down || move.sameDirection < maxSteps) &&
                  (move.direction == Direction.Down || move.sameDirection >= minSteps)
                ) {
                  val directionSteps: Int = if move.direction == Direction.Down then move.sameDirection + 1 else 1
                  Move(down, Direction.Down, directionSteps) -> (cost + field.get(down))
                }
              } ::
              field.left(move.pos).flatMap { (left: Pos) =>
                Option.when(
                  move.direction != Direction.Right && (move.direction != Direction.Left || move.sameDirection < maxSteps) &&
                  (move.direction == Direction.Left || move.sameDirection >= minSteps)
                ) {
                  val directionSteps: Int = if move.direction == Direction.Left then move.sameDirection + 1 else 1
                  Move(left, Direction.Left, directionSteps) -> (cost + field.get(left))
                }
              } ::
              field.right(move.pos).flatMap { (right: Pos) =>
                Option.when(
                  move.direction != Direction.Left && (move.direction != Direction.Right || move.sameDirection < maxSteps) &&
                  (move.direction == Direction.Right || move.sameDirection >= minSteps)
                ) {
                  val directionSteps: Int = if move.direction == Direction.Right then move.sameDirection + 1 else 1
                  Move(right, Direction.Right, directionSteps) -> (cost + field.get(right))
                }
              } :: Nil).flatten
          pq ++= newMoves
        }
        heatLoss_(visited + move)
      }

    heatLoss_(Set.empty)

  val task1: IO[Unit] = for {
    field: Vector[Vector[Int]] <- Utils.readLines[IO]("day17.input.txt").map(_.toVector.map(_.asDigit)).compile.toVector
    res: Int                    = heatLoss(field, 1, 3)
    _                          <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Vector[Vector[Int]] <- Utils.readLines[IO]("day17.input.txt").map(_.toVector.map(_.asDigit)).compile.toVector
    res: Int                    = heatLoss(field, 4, 10)
    _                          <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
