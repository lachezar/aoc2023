package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*

object Day4 extends IOApp.Simple {

  type CardId = Int
  type Count  = Int

  final case class Card(id: CardId, ticketNumber: Set[Int], winningNumbers: Set[Int]):
    val matches: Int = ticketNumber.intersect(winningNumbers).size

  object Card:
    def parse(input: String): Card =
      val parts: Array[String]    = input.split(":").map(_.trim)
      val numbers: List[Set[Int]] = parts(1).split("\\|").map(_.trim).map(_.split("\\s+").map(_.toInt).toSet).toList
      Card(parts(0).replaceAll("^Card\\s+", "").toInt, numbers(0), numbers(1))

  val task1: IO[Unit] =
    Utils
      .readLines[IO]("day4.input.txt")
      .evalMap { l =>
        IO.delay(Card.parse(l).matches).orRaise(new RuntimeException("invalid input"))
      }
      .map(matches => if matches == 0 then 0 else 1 << (matches - 1))
      .fold(0)(_ + _)
      .evalTap(IO.println)
      .compile
      .drain

  val task2: IO[Unit] = for {
    cards: Vector[Card]           <- Utils
                                       .readLines[IO]("day4.input.txt")
                                       .evalMap { l =>
                                         IO.delay(Card.parse(l)).orRaise(new RuntimeException("invalid input"))
                                       }
                                       .compile
                                       .toVector
    initialMap: Map[CardId, Count] = cards.map(_.id -> 1).toMap
    // for every card with id X and "card copies" count C,
    // for the number of "matches" M of that card X,
    // add C new "card copies" for cards with ids between X+1 .. X+M
    result: Map[CardId, Count]     = cards.foldLeft(initialMap) { (m: Map[CardId, Count], card: Card) =>
                                       val copiesCount: Count = m(card.id)
                                       ((card.id + 1) to (card.id + card.matches)).foldLeft(m) {
                                         (m: Map[CardId, Count], nextId: CardId) =>
                                           m.updatedWith(nextId)(_.map(_ + copiesCount))
                                       }
                                     }
    _                             <- IO.println(result.values.sum)
  } yield ()

  def run: IO[Unit] = task2
}
