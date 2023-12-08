package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.annotation.tailrec

object Day8 extends IOApp.Simple {

  def parseMap(lines: List[String]): Map[String, (String, String)] =
    lines.foldLeft(Map.empty) { (m: Map[String, (String, String)], l: String) =>
      l match
        case s"$key = ($left, $right)" => m.updated(key, left -> right)
        case _                         => m
    }

  def exploreMapOnce(start: String, map: Map[String, (String, String)], directions: String): String =
    directions.foldLeft(start)((pos: String, d: Char) => map(pos).direction(d))

  @tailrec
  def exploreMap(start: String, map: Map[String, (String, String)], directions: String, steps: Int): Int =
    exploreMapOnce(start, map, directions) match
      case "ZZZ" => steps + directions.length
      case other => exploreMap(other, map, directions, steps + directions.length)

  @tailrec
  def exploreMapAsGhost(pos: String, map: Map[String, (String, String)], directions: LazyList[Char], steps: Int): Int =
    map(pos).direction(directions.head) match
      case newPos if newPos.endsWith("Z") => steps + 1
      case other                          => exploreMapAsGhost(other, map, directions.tail, steps + 1)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  extension (t: Tuple2[String, String])
    def direction: Char => String =
      case 'L' => t._1
      case _   => t._2

  extension [A](l: List[A]) def repeatForever: LazyList[A] = l.to(LazyList) #::: l.repeatForever

  val task1: IO[Unit] = for {
    lines: List[String]               <- Utils.readLines[IO]("day8.input.txt").compile.toList
    directions: String                 = lines.head
    map: Map[String, (String, String)] = parseMap(lines.drop(2))
    _                                 <- IO.println(exploreMap("AAA", map, directions, 0))
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]               <- Utils.readLines[IO]("day8.input.txt").compile.toList
    directions: LazyList[Char]         = lines.head.toList.repeatForever
    map: Map[String, (String, String)] = parseMap(lines.drop(2))
    steps: List[Int]                   = map.keys.filter(_.endsWith("A")).map(exploreMapAsGhost(_, map, directions, 0)).toList
    denominator: Long                  = steps.reduce(gcd).toLong
    res: Long                          = steps.map(_.toLong).map(_ / denominator).product * denominator
    _                                 <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
