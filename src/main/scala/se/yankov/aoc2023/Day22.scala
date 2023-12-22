package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day22 extends IOApp.Simple {

  final case class ElfRange(from: Int, to: Int):
    def overlap(other: ElfRange): Boolean =
      (from <= other.from && other.from <= to) || (from <= other.to && other.to <= to) ||
      (other.from <= from && from <= other.to) || (other.from <= to && to <= other.to)
    def range: Range                      = this.from to this.to

  final case class Pos(x: Int, y: Int, z: Int)

  final case class Brick(id: Int, p1: Pos, p2: Pos):
    val height: Int                       = p2.z - p1.z + 1
    val xrange: ElfRange                  = ElfRange(p1.x, p2.x)
    val yrange: ElfRange                  = ElfRange(p1.y, p2.y)
    val zrange: ElfRange                  = ElfRange(p1.z, p2.z)
    def cartesianProduct: Seq[(Int, Int)] = yrange.range.flatMap((y: Int) => xrange.range.map(y -> _))
    def overlap(other: Brick): Boolean    =
      xrange.overlap(other.xrange) && yrange.overlap(other.yrange) && zrange.overlap(other.zrange)
    def offsetZ(value: Int): Brick        = copy(p1 = p1.copy(z = value + 1), p2 = p2.copy(z = value + height))

  def stackBricks(bricks: Vector[Brick]): Vector[Brick] =
    val bottom: Vector[Vector[Int]] =
      Vector.fill(bricks.map(_.p2.y).max + 1)(Vector.fill(bricks.map(_.p2.x).max + 1)(0))
    bricks
      .foldLeft(Vector.empty[Brick] -> bottom) {
        case (((brickAcc: Vector[Brick]) -> (botAcc: Vector[Vector[Int]])) -> (brick: Brick)) =>
          val highestPoint: Int = brick.cartesianProduct.map { case (y: Int) -> (x: Int) => botAcc(y)(x) }.max
          (brickAcc :+ brick.offsetZ(highestPoint)) ->
            brick.cartesianProduct.foldLeft(botAcc) {
              case ((botAcc: Vector[Vector[Int]]) -> ((y: Int) -> (x: Int))) =>
                botAcc.updated(y, botAcc(y).updated(x, highestPoint + brick.height))
            }
      }
      .head

  val task1: IO[Unit] = for {
    bricks: Vector[Brick]         <-
      Utils
        .readLines[IO]("day22.input.txt")
        .zipWithIndex
        .collect {
          case s"$x1,$y1,$z1~$x2,$y2,$z2" -> i =>
            Brick(i.toInt + 1, Pos(x1.toInt, y1.toInt, z1.toInt), Pos(x2.toInt, y2.toInt, z2.toInt))
        }
        .compile
        .toVector
        .map(_.sortBy(_.p1.z))
    normalizedStack: Vector[Brick] = stackBricks(bricks)
    res: Int                       = normalizedStack
                                       .combinations(normalizedStack.length - 1)
                                       .count((allButOne: Vector[Brick]) => stackBricks(allButOne) == allButOne)
    _                             <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    bricks: Vector[Brick]         <-
      Utils
        .readLines[IO]("day22.input.txt")
        .zipWithIndex
        .collect {
          case s"$x1,$y1,$z1~$x2,$y2,$z2" -> i =>
            Brick(i.toInt + 1, Pos(x1.toInt, y1.toInt, z1.toInt), Pos(x2.toInt, y2.toInt, z2.toInt))
        }
        .compile
        .toVector
        .map(_.sortBy(_.p1.z))
    normalizedStack: Vector[Brick] = stackBricks(bricks)
    res: Int                       = normalizedStack
                                       .combinations(normalizedStack.length - 1)
                                       .map((allButOne: Vector[Brick]) => (allButOne.toSet -- stackBricks(allButOne).toSet).size)
                                       .sum
    _                             <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
