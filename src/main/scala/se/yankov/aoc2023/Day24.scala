package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day24 extends IOApp.Simple {

  final case class Pos[A](x: A, y: A, z: A)

  final case class Velocity[A](x: A, y: A, z: A)

  // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection

  // (x1y2-y1x2)(x3-x4)-(x1-x2)(x3y4-y3x4)
  //  -----------------------------------
  // (x1-x2)(y3-y4)-(y1-y2)(x3-x4)

  // (x1y2-y1x2)(y3-y4)-(y1-y2)(x3y4-y3x4)
  // ------------------------------------
  // (x1-x2)(y3-y4)-(y1-y2)(x3-x4)

  val task1: IO[Unit] = for {
    lines: List[(Pos[Long], Velocity[Long])] <-
      Utils
        .readLines[IO]("day24.input.txt")
        .collect {
          case s"$px, $py, $pz @ $vx, $vy, $vz" =>
            Pos(px.toLong, py.toLong, pz.toLong) -> Velocity(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
        }
        .compile
        .toList
    minLimit: Long                            = 200_000_000_000_000L
    maxLimit: Long                            = 400_000_000_000_000L
    res: Int                                  = lines
                                                  .combinations(2)
                                                  .collect {
                                                    case (p1: Pos[Long], v1: Velocity[Long]) :: (p2: Pos[Long], v2: Velocity[Long]) :: Nil =>
                                                      val x1: BigDecimal          = BigDecimal(p1.x)
                                                      val y1: BigDecimal          = BigDecimal(p1.y)
                                                      val x2: BigDecimal          = BigDecimal(p1.x + v1.x)
                                                      val y2: BigDecimal          = BigDecimal(p1.y + v1.y)
                                                      val x3: BigDecimal          = BigDecimal(p2.x)
                                                      val y3: BigDecimal          = BigDecimal(p2.y)
                                                      val x4: BigDecimal          = BigDecimal(p2.x + v2.x)
                                                      val y4: BigDecimal          = BigDecimal(p2.y + v2.y)
                                                      val denominator: BigDecimal = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
                                                      if denominator == 0 then None // parallel or same line
                                                      else
                                                        val cx: BigDecimal =
                                                          ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / denominator
                                                        val cy: BigDecimal =
                                                          ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / denominator
                                                        Option.when(
                                                          ((v1.x < 0 && cx < p1.x) || (v1.x > 0 && cx > p1.x)) &&
                                                          ((v2.x < 0 && cx < p2.x) || (v2.x > 0 && cx > p2.x)) &&
                                                          ((v1.y < 0 && cy < p1.y) || (v1.y > 0 && cy > p1.y)) &&
                                                          ((v2.y < 0 && cy < p2.y) || (v2.y > 0 && cy > p2.y)) &&
                                                          minLimit <= cx && cx <= maxLimit && minLimit <= cy && cy <= maxLimit
                                                        )(cx -> cy)
                                                  }
                                                  .flatten
                                                  .length
    _                                        <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1
}
