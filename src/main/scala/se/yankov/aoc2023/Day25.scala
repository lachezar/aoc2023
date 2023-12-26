package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day25 extends IOApp.Simple {

  final case class Edge(a: String, b: String):
    override def equals(other: Any): Boolean =
      if !other.isInstanceOf[Edge] then false
      else
        val otherEdge: Edge = other.asInstanceOf[Edge]
        (otherEdge.a == a && otherEdge.b == b) || (otherEdge.a == b && otherEdge.b == a)

  extension (edges: List[Edge])
    def toGraph: Map[String, List[String]] = edges.flatMap((e: Edge) => e :: Edge(e.b, e.a) :: Nil).groupMap(_.a)(_.b)

  extension (nodes: List[String])
    def toEdges: List[Edge] = nodes.sliding(2).collect { case (a :: b :: Nil) => Edge(a, b) }.toList

  def fillWithMaxPath(graph: Map[String, List[String]], queue: Vector[String], visited: Set[String], path: List[String])
      : List[String] =
    queue match
      case (node: String) +: (next: Vector[String]) =>
        if visited.contains(node) then fillWithMaxPath(graph, next, visited, path)
        else fillWithMaxPath(graph, next :++ graph(node), visited + node, node :: path)
      case _                                        => path

  def walkWithPath(graph: Map[String, List[String]], queue: Vector[(String, List[String])], endNode: String)
      : List[String] =
    queue match
      case (node: String, path: List[String]) +: (next: Vector[(String, List[String])]) =>
        if node == endNode then node :: path
        else if path.contains(node) then walkWithPath(graph, next, endNode)
        else walkWithPath(graph, next :++ graph(node).map(_ -> (node :: path)), endNode)
      case _                                                                            => Nil

  def run: IO[Unit] = for {
    edges: List[Edge]               <- Utils
                                         .readLines[IO]("day25.input.txt")
                                         .map { (line: String) =>
                                           val parts: Array[String] = line.split(": ")
                                           parts(0) -> parts(1).split(' ').toList
                                         }
                                         .compile
                                         .toList
                                         .map(_.flatMap((a, bs) => bs.map(Edge(a, _))))
    graph: Map[String, List[String]] = edges.toGraph
    maxPath: List[String]            = fillWithMaxPath(graph, Vector(edges.head.a), Set.empty, Nil)
    startNodes: Vector[String]       = fillWithMaxPath(graph, Vector(maxPath.head), Set.empty, Nil).toVector
    offset: Int                      = Math.sqrt(Math.sqrt(startNodes.length)).toInt // what value should I use?
    pathA: List[String]              = walkWithPath(graph, Vector(startNodes.head -> Nil), maxPath.head)
    pathB: List[String]              = walkWithPath(graph, Vector(startNodes(offset) -> pathA), maxPath.head)
    pathC: List[String]              = walkWithPath(graph, Vector(startNodes(2 * offset) -> pathB), maxPath.head)
    subsetMaxPath: Int               = (for {
                                         a: Edge <- pathA.toEdges
                                         b: Edge <- pathB.toEdges
                                         c: Edge <- pathC.toEdges
                                       } yield {
                                         val subset: List[Edge] = edges.filterNot((e: Edge) => a == e || b == e || c == e)
                                         fillWithMaxPath(subset.toGraph, Vector(startNodes.head), Set.empty, Nil).size
                                       }).min
    res: Int                         = (maxPath.length - subsetMaxPath) * subsetMaxPath
    _                               <- IO.println(res)
  } yield ()
}
