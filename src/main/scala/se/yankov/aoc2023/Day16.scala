package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec

object Day16 extends IOApp.Simple {

  enum Direction(val y: Int, val x: Int):
    case Left  extends Direction(0, -1)
    case Right extends Direction(0, 1)
    case Up    extends Direction(-1, 0)
    case Down  extends Direction(1, 0)

  final case class Pos(y: Int, x: Int)

  final case class Move(pos: Pos, direction: Direction):
    def nextMove: Move                          = copy(pos = Pos(pos.y + direction.y, pos.x + direction.x))
    def nextMove(newDirection: Direction): Move =
      Move(Pos(pos.y + newDirection.y, pos.x + newDirection.x), newDirection)

  extension (field: Vector[Vector[Char]])
    def get(pos: Pos): Char          = field(pos.y)(pos.x)
    def isInvalid(pos: Pos): Boolean = pos.y < 0 || pos.y >= field.length || pos.x < 0 || pos.x >= field(0).length

  @tailrec
  def energizedTiles(field: Vector[Vector[Char]], stack: List[Move], visited: Set[Move]): Int =
    stack match
      case Nil                             => visited.map(_.pos).size
      case (m: Move) :: (rest: List[Move]) =>
        if visited.contains(m) || field.isInvalid(m.pos) then energizedTiles(field, rest, visited)
        else
          val next: List[Move] = field.get(m.pos) match
            case '.'  => m.nextMove :: rest
            case '|'  =>
              m.direction match
                case Direction.Left | Direction.Right => m.nextMove(Direction.Up) :: m.nextMove(Direction.Down) :: rest
                case Direction.Up | Direction.Down    => m.nextMove :: rest
            case '-'  =>
              m.direction match
                case Direction.Up | Direction.Down    => m.nextMove(Direction.Left) :: m.nextMove(Direction.Right) :: rest
                case Direction.Left | Direction.Right => m.nextMove :: rest
            case '\\' =>
              m.direction match
                case Direction.Left  => m.nextMove(Direction.Up) :: rest
                case Direction.Right => m.nextMove(Direction.Down) :: rest
                case Direction.Up    => m.nextMove(Direction.Left) :: rest
                case Direction.Down  => m.nextMove(Direction.Right) :: rest
            case '/'  =>
              m.direction match
                case Direction.Left  => m.nextMove(Direction.Down) :: rest
                case Direction.Right => m.nextMove(Direction.Up) :: rest
                case Direction.Up    => m.nextMove(Direction.Right) :: rest
                case Direction.Down  => m.nextMove(Direction.Left) :: rest
            case _    => rest
          energizedTiles(field, next, visited + m)

  val task1: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day16.input.txt").map(_.toVector).compile.toVector
    res: Int                     = energizedTiles(field, Move(Pos(0, 0), Direction.Right) :: Nil, Set.empty)
    _                           <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day16.input.txt").map(_.toVector).compile.toVector
    res: Int                     = (0 until field.length).flatMap { (i: Int) =>
                                     energizedTiles(field, Move(Pos(0, i), Direction.Down) :: Nil, Set.empty) ::
                                       energizedTiles(field, Move(Pos(field.length - 1, i), Direction.Up) :: Nil, Set.empty) ::
                                       energizedTiles(field, Move(Pos(i, 0), Direction.Right) :: Nil, Set.empty) ::
                                       energizedTiles(field, Move(Pos(i, field.length - 1), Direction.Left) :: Nil, Set.empty) :: Nil
                                   }.max
    _                           <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
