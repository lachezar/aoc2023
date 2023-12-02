package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*

object Day2 extends IOApp.Simple {

  final case class Game(id: Int, picks: List[Pick])

  object Game:
    def parse(input: String): Game =
      val parts: Array[String] = input.split(":").map(_.trim)
      Game(parts(0).drop("Game ".length).toInt, parts(1).split(";").map(_.trim).map(Pick.parse).toList)

  final case class Pick(red: Int, green: Int, blue: Int)

  object Pick:
    def empty: Pick                = Pick(0, 0, 0)
    def parse(input: String): Pick =
      input
        .split(",")
        .map(_.trim.split(" "))
        .foldLeft(Pick.empty) { (pick, parts) =>
          parts(1) match {
            case "red"   => pick.copy(red = parts(0).toInt)
            case "green" => pick.copy(green = parts(0).toInt)
            case "blue"  => pick.copy(blue = parts(0).toInt)
          }
        }

  val task1: IO[Unit] =
    Utils
      .readLines[IO]("day2.input.txt")
      .map { l =>
        val game: Game = Game.parse(l)
        if game.picks.exists(pick => pick.red > 12 || pick.green > 13 || pick.blue > 14) then 0 else game.id
      }
      .fold(0)(_ + _)
      .evalTap(IO.println)
      .compile
      .drain

  val task2: IO[Unit] =
    Utils
      .readLines[IO]("day2.input.txt")
      .map { l =>
        val game: Game            = Game.parse(l)
        val minPossiblePick: Pick = game
          .picks
          .reduce((a, b) => Pick(Math.max(a.red, b.red), Math.max(a.green, b.green), Math.max(a.blue, b.blue)))
        minPossiblePick.red * minPossiblePick.green * minPossiblePick.blue
      }
      .fold(0)(_ + _)
      .evalTap(IO.println)
      .compile
      .drain

  def run: IO[Unit] = task2
}
