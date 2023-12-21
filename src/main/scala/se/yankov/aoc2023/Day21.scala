package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day21 extends IOApp.Simple {

  final case class Pos(y: Int, x: Int):
    def nextSteps: Seq[Pos]                      = Pos(y + 1, x) :: Pos(y - 1, x) :: Pos(y, x + 1) :: Pos(y, x - 1) :: Nil
    def replicate(n: Int, period: Int): Seq[Pos] =
      (for {
        offsetY <- 0 to n
        offsetX <- 0 to n
      } yield Pos(y + offsetY * period, x + offsetX * period) ::
        Pos(y + offsetY * period, x - offsetX * period) ::
        Pos(y - offsetY * period, x + offsetX * period) ::
        Pos(y - offsetY * period, x - offsetX * period) :: Nil).flatten

  def step(pos: Set[Pos], rocks: Set[Pos]): Set[Pos] = pos.flatMap(_.nextSteps) -- rocks

  def rocksPositions(lines: List[String]): Set[Pos] =
    lines.zipWithIndex.flatMap { case line -> y => line.zipWithIndex.collect { case '#' -> x => Pos(y, x) } }.toSet

  def startPosition(lines: List[String]): Pos =
    lines.zipWithIndex.collectFirst { case line -> y if line.contains('S') => Pos(y, line.indexOf("S")) }.get

  def deriveElementInSequence(arr: Array[Long], index: Int): Long =
    if index >= 0 then deriveElementInSequence(arr.tail :+ Day9.findNext(arr), index - 1)
    else arr.last

  val task1: IO[Unit] = for {
    lines: List[String] <- Utils.readLines[IO]("day21.input.txt").compile.toList
    start: Pos           = startPosition(lines)
    rocks: Set[Pos]      = rocksPositions(lines)
    res: Set[Pos]        = (1 to 64).foldLeft(Set(start)) { case (acc: Set[Pos], _) => step(acc, rocks) }
    _                   <- IO.println(res.size)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]           <- Utils.readLines[IO]("day21.input.txt").compile.toList
    start: Pos                     = startPosition(lines)
    rocks: Set[Pos]                = rocksPositions(lines).flatMap(_.replicate(3, lines.length))
    (_, coefficients: Array[Long]) = (1 to (lines.length * 7 / 2)).foldLeft(Set(start) -> Array.empty[Long]) {
                                       case ((accPos: Set[Pos], accCoefficients: Array[Long]), i) =>
                                         val res: Set[Pos] = step(accPos, rocks)
                                         if i      % lines.length == lines.length / 2 then
                                           res    -> (accCoefficients :+ res.size)
                                         else res -> accCoefficients
                                     }
    res: Long                      = deriveElementInSequence(coefficients.toArray, (26501365 - 65) / lines.length - coefficients.length)
    _                             <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
