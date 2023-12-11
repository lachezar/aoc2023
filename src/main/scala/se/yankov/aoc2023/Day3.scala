package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import cats.syntax.all.*

object Day3 extends IOApp.Simple {

  type Board = Array[Array[Char]]

  final case class Number(value: Int, row: Int, start: Int, end: Int):
    def isPart(board: Board): Boolean =
      (start > 0 && board(row)(start - 1) != '.') || (end < board(0).length - 1 && board(row)(end + 1) != '.') ||
      (start > 0 && row > 0 && board(row - 1)(start - 1) != '.') || (end < board(0).length - 1 && row > 0 && board(
        row - 1
      )(end + 1) != '.') ||
      (start > 0 && row < board.length - 1 && board(row + 1)(start - 1) != '.') || (end < board(
        0
      ).length - 1 && row < board.length - 1 && board(row + 1)(end + 1) != '.') ||
      (row > 0 && (start to end).exists(board(row - 1)(_) != '.')) || (row < board.length - 1 && (start to end).exists(
        board(row + 1)(_) != '.'
      ))

    def cog(board: Board): Option[(Int, Int)] =
      Option.when(start > 0 && board(row)(start - 1) == '*')(row -> (start - 1)) orElse
        Option.when(end < board(0).length - 1 && board(row)(end + 1) == '*')(row -> (end + 1)) orElse
        Option.when(start > 0 && row > 0 && board(row - 1)(start - 1) == '*')((row - 1) -> (start - 1)) orElse
        Option
          .when(end < board(0).length - 1 && row > 0 && board(row - 1)(end + 1) == '*')((row - 1) -> (end + 1)) orElse
        Option.when(start > 0 && row < board.length - 1 && board(row + 1)(start - 1) == '*')(
          (row + 1) -> (start - 1)
        ) orElse
        Option.when(end < board(0).length - 1 && row < board.length - 1 && board(row + 1)(end + 1) == '*')(
          (row + 1) -> (end + 1)
        ) orElse
        (if row > 0 then (start to end).find(board(row - 1)(_) == '*').map((row - 1) -> _) else None) orElse
        (if row < board.length - 1 then (start to end).find(board(row + 1)(_) == '*').map((row + 1) -> _)
         else None)

    def addDigit(d: Int): Number = copy(value = value * 10 + d, end = end + 1)

  object Number:
    def empty: Number = Number(0, 0, 0, 0)

  def findNumbers(board: Board, number: Option[Number], pos: Int, parts: List[Number]): List[Number] =
    if pos >= board.length * board(0).length then parts
    else {
      val y: Int                                                        = pos / board(0).length
      val x: Int                                                        = pos % board(0).length
      val cell: Char                                                    = board(y)(x)
      val (currentNumber, currentParts): (Option[Number], List[Number]) = if (x == 0) {
        None -> (number.find(_.isPart(board)).toList ++ parts)
      }
      else number -> parts
      val (newNumber, newParts): (Option[Number], List[Number])         = if (cell.isDigit) {
        val digit: Int = cell - '0'
        currentNumber match
          case None        => Some(Number(digit, y, x, x)) -> currentParts
          case Some(value) => Some(value.addDigit(digit))  -> currentParts
      }
      else {
        None -> (currentNumber.find(_.isPart(board)).toList ++ currentParts)
      }
      findNumbers(board, newNumber, pos + 1, newParts)
    }

  val task1: IO[Unit] = for {
    lines: List[String] <- Utils.readLines[IO]("day3.input.txt").compile.toList
    board: Board         = lines.map(_.toCharArray()).toArray
    parts: Int          <- IO(findNumbers(board, None, 0, Nil).foldLeft(0)((acc: Int, n: Number) => acc + n.value))
                             .orRaise(new RuntimeException("invalid input"))
    _                   <- IO.println(parts)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]                                 <- Utils.readLines[IO]("day3.input.txt").compile.toList
    board: Board                                         = lines.map(_.toCharArray()).toArray
    parts: List[Number]                                 <- IO(findNumbers(board, None, 0, Nil)).orRaise(new RuntimeException("invalid input"))
    partsWithCogs: Map[Option[(Int, Int)], List[Number]] = parts.groupBy(_.cog(board)).filter {
                                                             case Some(y -> x) -> (n1 :: n2 :: Nil) => true
                                                             case _                                 => false
                                                           }
    gearRatios: Int                                      = partsWithCogs.values.map(_.map(_.value).product).sum
    _                                                   <- IO.println(gearRatios)
  } yield ()

  def run: IO[Unit] = task2
}
