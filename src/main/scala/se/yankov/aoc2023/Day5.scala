package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*

object Day5 extends IOApp.Simple {

  final case class ElfRange(start: Long, end: Long):
    def contains(value: Long): Boolean     = start <= value && value < end
    def overlaps(other: ElfRange): Boolean =
      start <= other.start && other.start < end || start < other.end && other.end <= end

  final case class Jump(range: ElfRange, change: Option[Long]):
    def normalize: Jump =
      Jump(ElfRange(range.start + change.getOrElse(0L), range.end + change.getOrElse(0L)), None)

  object Jump:
    def parse(line: String): Jump =
      val res: Array[Long] = line.split(" ").map(_.toLong)
      Jump(ElfRange(res(1), res(1) + res(2)), Some(res(0) - res(1)))

  final case class ElfMap(name: String, jumps: List[Jump]):
    def get(from: Long): Long =
      jumps
        .find((j: Jump) => j.range.contains(from))
        .fold(from)((j: Jump) => from + j.change.getOrElse(0L))

  object ElfMap:
    def parse(lines: List[String]): ElfMap =
      ElfMap(lines.head.dropRight(" map:".length), lines.tail.map(Jump.parse))

  def parseSeeds(line: String): Array[Long] = line.drop("seeds: ".length).split(" ").map(_.toLong)

  def parseSeedRanges(line: String): List[ElfRange] =
    line
      .drop("seeds: ".length)
      .split(" ")
      .map(_.toLong)
      .sliding(2, 2)
      .map((pair: Array[Long]) => ElfRange(pair(0), pair(0) + pair(1)))
      .toList

  def partitionByEmptyLine(lines: List[String]): List[List[String]] =
    lines
      .foldLeft(List.apply[List[String]](Nil)) {
        case acc -> ""           => Nil :: acc
        case (hacc :: tacc) -> l => (l :: hacc) :: tacc
        case Nil -> l            => (l :: Nil) :: Nil
      }
      .map(_.reverse)
      .reverse

  def intersectRanges(target: Jump, intersector: Jump): List[Jump] =
    if intersector.range.start >= target.range.end || intersector.range.end <= target.range.start then target :: Nil
    else if intersector.range.start <= target.range.start && intersector.range.end >= target.range.end then
      Jump(ElfRange(target.range.start, target.range.end), intersector.change) :: Nil
    else if intersector.range.start <= target.range.start && intersector.range.end < target.range.end then
      Jump(ElfRange(target.range.start, intersector.range.end), intersector.change) ::
        Jump(ElfRange(intersector.range.end, target.range.end), None) ::
        Nil
    else if intersector.range.start > target.range.start && intersector.range.end >= target.range.end then
      Jump(ElfRange(intersector.range.start, target.range.end), intersector.change) ::
        Jump(ElfRange(target.range.start, intersector.range.start), None) ::
        Nil
    else if intersector.range.start > target.range.start && intersector.range.end < target.range.end then
      Jump(ElfRange(intersector.range.start, intersector.range.end), intersector.change) ::
        Jump(ElfRange(target.range.start, intersector.range.start), None) ::
        Jump(ElfRange(intersector.range.end, target.range.end), None) ::
        Nil
    else
      throw new RuntimeException("UNHANDLED")
      Nil

  val task1: IO[Unit] = for {
    lines: List[String] <- Utils.readLines[IO]("day5.input.txt").compile.toList
    seeds: Array[Long]   = parseSeeds(lines.head)
    maps: List[ElfMap]   = partitionByEmptyLine(lines.drop(2)).map(ElfMap.parse)
    res: Long            = seeds.map((seed: Long) => maps.foldLeft(seed)((acc: Long, m: ElfMap) => m.get(acc))).min
    _                   <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]   <- Utils.readLines[IO]("day5.input.txt").compile.toList
    seeds: List[ElfRange]  = parseSeedRanges(lines.head)
    jumpMaps: List[ElfMap] = partitionByEmptyLine(lines.drop(2)).map(ElfMap.parse)
    res: List[Jump]        =
      jumpMaps
        .map(_.jumps)
        .foldLeft(seeds.map(Jump(_, None)))((acc: List[Jump], jm: List[Jump]) =>
          val intersections: List[Jump]                     = acc.flatMap((target: Jump) => jm.flatMap(intersectRanges(target, _)))
          val (specified: List[Jump], defaults: List[Jump]) = intersections.partition(_.change.isDefined)
          val removedShadowedDefaults: List[Jump]           =
            specified ++ defaults.filterNot((d: Jump) => specified.exists((s: Jump) => d.range.overlaps(s.range)))
          removedShadowedDefaults.map(_.normalize)
        )
    _                     <- IO.println(res.map(_.range.start).min)
  } yield ()

  def run: IO[Unit] = task2
}
