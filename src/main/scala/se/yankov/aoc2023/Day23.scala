package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec

object Day23 extends IOApp.Simple {

  extension (field: Vector[Vector[Char]])
    def get(pos: Pos): Char            = field(pos.y)(pos.x)
    def isCrossroad(pos: Pos): Boolean =
      (field(pos.y - 1)(pos.x) :: field(pos.y + 1)(pos.x) :: field(pos.y)(pos.x - 1) :: field(pos.y)(pos.x + 1) :: Nil)
        .filter(">v".contains(_))
        .length >= 3

  final case class Pos(y: Int, x: Int)

  final case class Node(p1: Pos, p2: Pos, weight: Int)

  def nextMovesWithSlopes(pos: Pos, field: Vector[Vector[Char]]): Vector[Pos] =
    (field.get(pos) match
      case '.' =>
        (if pos.y > 0 then Vector(pos.copy(y = pos.y - 1)) else Vector.empty) ++
          (if pos.y < field.length - 1 then Vector(pos.copy(y = pos.y + 1)) else Vector.empty) ++
          (if pos.x > 0 then Vector(pos.copy(x = pos.x - 1)) else Vector.empty) ++
          (if pos.x < field(0).length then Vector(pos.copy(x = pos.x + 1)) else Vector.empty)
      case 'v' => if pos.y < field.length - 1 then Vector(pos.copy(y = pos.y + 1)) else Vector.empty
      case '>' => if pos.x < field(0).length then Vector(pos.copy(x = pos.x + 1)) else Vector.empty
    ).filterNot((pos: Pos) => field.get(pos) == '#')

  def nextMovesIgnoreSlopes(pos: Pos, field: Vector[Vector[Char]]): Vector[Pos] =
    (if field.get(pos) != '#' then
       (if pos.y > 0 then Vector(pos.copy(y = pos.y - 1)) else Vector.empty) ++
         (if pos.y < field.length - 1 then Vector(pos.copy(y = pos.y + 1)) else Vector.empty) ++
         (if pos.x > 0 then Vector(pos.copy(x = pos.x - 1)) else Vector.empty) ++
         (if pos.x < field(0).length then Vector(pos.copy(x = pos.x + 1)) else Vector.empty)
     else Vector.empty).filterNot((pos: Pos) => field.get(pos) == '#')

  @tailrec
  def walk(field: Vector[Vector[Char]], queue: Vector[(Pos, Set[Pos])], maxPath: Int): Int =
    queue match
      case ((pos: Pos) -> (visited: Set[Pos])) +: (next: Vector[(Pos, Set[Pos])]) =>
        walk(
          field,
          next :++ nextMovesWithSlopes(pos, field).filterNot(visited.contains).map(_ -> (visited + pos)),
          if pos.y == field.length - 1 then Math.max(maxPath, visited.size) else maxPath,
        )
      case _                                                                      => maxPath

  @tailrec
  def walkBetweenCrossroads(
      field: Vector[Vector[Char]],
      queue: Vector[(Pos, Set[Pos])],
      startPos: Pos,
      crossroads: Set[Pos],
      nodes: List[Node],
    ): List[Node] =
    queue match
      case ((pos: Pos) -> (visited: Set[Pos])) +: (next: Vector[(Pos, Set[Pos])]) =>
        if crossroads.contains(pos) && pos != startPos then
          walkBetweenCrossroads(field, next, startPos, crossroads, Node(startPos, pos, visited.size) :: nodes)
        else
          walkBetweenCrossroads(
            field,
            next :++ nextMovesIgnoreSlopes(pos, field).filterNot(visited.contains(_)).map((_, visited + pos)),
            startPos,
            crossroads,
            nodes,
          )
      case _                                                                      => nodes

  @tailrec
  def traverseGraph(graph: List[Node], stack: List[(Node, Set[Pos], Int)], endPos: Pos, maxWeight: Int): Int =
    stack match
      case ((node: Node), (visited: Set[Pos]), (pathWeight: Int)) :: (next: List[(Node, Set[Pos], Int)]) =>
        if visited.contains(node.p2) then traverseGraph(graph, next, endPos, maxWeight)
        else
          traverseGraph(
            graph,
            graph.filter(_.p1 == node.p2).map((_, (visited + node.p1), pathWeight + node.weight)) ++ next,
            endPos,
            if node.p2 == endPos then Math.max(maxWeight, pathWeight + node.weight) else maxWeight,
          )
      case _                                                                                             => maxWeight

  val task1: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day23.input.txt").map(_.toVector).compile.toVector
    startPos: Pos                = Pos(0, field.head.indexOf('.'))
    endPos: Pos                  = Pos(field.length - 1, field.last.indexOf('.'))
    res: Int                     = walk(field, Vector(startPos -> Set.empty), 0)
    _                           <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    field: Vector[Vector[Char]] <- Utils.readLines[IO]("day23.input.txt").map(_.toVector).compile.toVector
    startPos: Pos                = Pos(0, field.head.indexOf('.'))
    endPos: Pos                  = Pos(field.length - 1, field.last.indexOf('.'))
    crossroads: List[Pos]        = (for {
                                     y: Int <- 1 until field.length - 1
                                     x: Int <- 1 until field(0).length - 1
                                   } yield Pos(y, x)).filter(field.isCrossroad).toList
    directedGraph: List[Node]    =
      (startPos :: crossroads).flatMap((pos: Pos) =>
        walkBetweenCrossroads(field, Vector(pos -> Set.empty), pos, (endPos :: crossroads).toSet, Nil)
      )
    longestPath: Int             =
      traverseGraph(directedGraph, directedGraph.filter(_.p1 == startPos).map((_, Set.empty, 0)), endPos, 0)
    _                           <- IO.println(longestPath)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
