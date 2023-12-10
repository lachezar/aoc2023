package se.yankov.aoc2023

import Utils.dbg
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import scala.annotation.tailrec

object Day10 extends IOApp.Simple {

  final case class Pos(y: Int, x: Int)

  @tailrec
  def walk(field: Array[Array[Char]], stack: List[Pos], visited: Set[Pos]): Set[Pos] =
    stack match
      case Nil         =>
        visited
      case pos :: rest =>
        val nextPos: List[Pos] = field.nextPipe(pos).filterNot(visited.contains(_)).filterNot(field.get(_) == '.')
        walk(field, nextPos ++ rest, visited + pos)

  @tailrec
  def fill(field: Array[Array[Char]], stack: List[Pos], visited: Set[Pos]): Set[Pos] =
    stack match
      case Nil         =>
        visited
      case pos :: rest =>
        val nextPos: List[Pos] = field.nextAny(pos).filterNot(visited.contains(_)).filter(field.get(_) == '.')
        fill(field, nextPos ++ rest, visited + pos)

  extension (field: Array[Array[Char]]) def get(p: Pos): Char = field(p.y)(p.x)
  extension (field: Array[Array[Char]])
    def nextPipe(p: Pos): List[Pos]                           =
      val pipe: Char = field.get(p)
      (if p.x > 0 && List('J', '7', '-', 'S').contains(pipe) then Pos(p.y, p.x - 1) :: Nil else Nil) ++
        (if p.x < field(0).length - 1 && List('F', 'L', '-', 'S').contains(pipe) then Pos(p.y, p.x + 1) :: Nil
         else Nil) ++
        (if p.y > 0 && List('J', 'L', '|', 'S').contains(pipe) then Pos(p.y - 1, p.x) :: Nil else Nil) ++
        (if p.y < field.length - 1 && List('F', '7', '|', 'S').contains(pipe) then Pos(p.y + 1, p.x) :: Nil else Nil) ++
        Nil
  extension (field: Array[Array[Char]])
    def nextAny(p: Pos): List[Pos]                            =
      (if p.x > 0 then Pos(p.y, p.x - 1) :: Nil else Nil) ++
        (if p.x < field(0).length - 1 then Pos(p.y, p.x + 1) :: Nil else Nil) ++
        (if p.y > 0 then Pos(p.y - 1, p.x) :: Nil else Nil) ++
        (if p.y < field.length - 1 then Pos(p.y + 1, p.x) :: Nil else Nil) ++
        Nil

  def expandSpace(field: Array[Array[Char]]): Array[Array[Char]] =
    Array.fill(field(0).length * 2 + 2)('.') +: field
      .map((row: Array[Char]) =>
        ('.' +: row :+ '.')
          .sliding(2)
          .map((parts: Array[Char]) => parts(0) -> parts(1))
          .flatMap {
            case '|' -> x => '|' :: '.' :: Nil
            case '-' -> x => '-' :: '-' :: Nil
            case 'F' -> x => 'F' :: '-' :: Nil
            case '7' -> x => '7' :: '.' :: Nil
            case 'J' -> x => 'J' :: '.' :: Nil
            case 'L' -> x => 'L' :: '-' :: Nil
            case x -> y   => x :: x :: Nil
          }
          .toArray
      )
      .flatMap((row: Array[Char]) =>
        row :: row.map {
          case '-' => '.'
          case 'L' => '.'
          case 'F' => '|'
          case 'J' => '.'
          case '7' => '|'
          case x   => x
        }
          :: Nil
      )

  def contractSpace(field: Array[Array[Char]]): Array[Array[Char]] =
    field
      .map((row: Array[Char]) =>
        row
          .sliding(2, 2)
          .map((parts: Array[Char]) => parts(0) -> parts(1))
          .flatMap {
            case '|' -> '.' => '|' :: Nil
            case '-' -> '-' => '-' :: Nil
            case 'F' -> '-' => 'F' :: Nil
            case '7' -> '.' => '7' :: Nil
            case 'J' -> '.' => 'J' :: Nil
            case 'L' -> '-' => 'L' :: Nil
            case x -> y     => x :: Nil
          }
          .toArray
      )
      .drop(1)
      .sliding(2, 2)
      .flatMap((row: Array[Array[Char]]) => row.head :: Nil)
      .toArray

  val task1and2: IO[Unit] = for {
    field: Array[Array[Char]]          <-
      Utils.readLines[IO]("day10.input.txt").map(_.toArray).compile.toVector.map(_.toArray)
    start: Pos                          = field
                                            .zipWithIndex
                                            .collectFirst {
                                              case (row: Array[Char], y: Int) if row.contains('S') => y -> row.indexOf('S')
                                            }
                                            .map(Pos.apply)
                                            .get
    loopVertices: Set[Pos]              = walk(field, start :: Nil, Set.empty) - start
    _                                  <- IO.println((loopVertices.size + 1) / 2)
    normalizedField: Array[Array[Char]] = field.map(_.map(_ => '.'))
    _                                   = loopVertices.foreach((p: Pos) => normalizedField(p.y)(p.x) = field(p.y)(p.x))
    _                                   = normalizedField(start.y)(start.x) = 'S'
    expandedField: Array[Array[Char]]   = expandSpace(normalizedField)
    outOfTheLoopPositions: Set[Pos]     = fill(expandedField, Pos(0, 0) :: Nil, Set.empty)
    _                                   = outOfTheLoopPositions.foreach((p: Pos) => expandedField(p.y)(p.x) = ' ')
    contractedField: Array[Array[Char]] = contractSpace(expandedField)
    innerLoopVertices: Int              = contractedField.map(_.count(_ == '.')).sum
    _                                  <- IO.println(innerLoopVertices)
  } yield ()

  def run: IO[Unit] = task1and2
}
