package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.chaining.scalaUtilChainingOps

object Day7 extends IOApp.Simple {

  enum HandType:
    case HighCard, OnePair, TwoPair, ThreeOfKind, FullHouse, FourOfKind, FiveOfKind

  object HandType:

    val classifyHelper: (List[Int]) => HandType = {
      case 5 :: _      => HandType.FiveOfKind
      case 4 :: _      => HandType.FourOfKind
      case 3 :: 2 :: _ => HandType.FullHouse
      case 3 :: _      => HandType.ThreeOfKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: _      => HandType.OnePair
      case _           => HandType.HighCard
    }

    def classify(cards: List[Int]): HandType =
      cards.groupBy(identity).values.toList.map(_.length).sorted(Ordering[Int].reverse) pipe classifyHelper

    def classifyByElf(cards: List[Int]): HandType =
      val (jokers, nonJokers): (List[Int], List[Int]) = cards.partition(_ == 11)
      nonJokers.groupBy(identity).values.toList.map(_.length).sorted(Ordering[Int].reverse) match
        case h :: t => ((h + jokers.length) :: t) pipe classifyHelper
        case Nil    => HandType.FiveOfKind

  final case class Hand(cards: List[Int], bid: Int, handType: HandType)

  object Hand:
    def parse(s: String, bid: String, classifier: List[Int] => HandType): Hand =
      val cards = s.map {
        case 'A' => 14
        case 'K' => 13
        case 'Q' => 12
        case 'J' => 11
        case 'T' => 10
        case d   => d - '0'
      }.toList
      Hand(cards, bid.toInt, classifier(cards))

  val task1: IO[Unit] = for {
    hands: List[Hand]      <- Utils
                                .readLines[IO]("day7.input.txt")
                                .map(_.split(' '))
                                .map(part => Hand.parse(part(0), part(1), HandType.classify))
                                .compile
                                .toList
    sortedHands: List[Hand] = hands.sortBy((h: Hand) => h.handType.ordinal :: h.cards)
    res: Int                = sortedHands.zipWithIndex.map((h, r) => h.bid * (r + 1)).sum
    _                      <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    hands: List[Hand]      <- Utils
                                .readLines[IO]("day7.input.txt")
                                .map(_.split(' '))
                                .map(part => Hand.parse(part(0), part(1), HandType.classifyByElf))
                                .compile
                                .toList
    sortedHands: List[Hand] =
      hands.sortBy((h: Hand) => h.handType.ordinal :: h.cards.map(c => if c == 11 then 1 else c))
    res: Int                = sortedHands.zipWithIndex.map((h, r) => h.bid * (r + 1)).sum
    _                      <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task2
}
