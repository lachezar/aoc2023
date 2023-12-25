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

  def intersectionPoint(p1: Pos[Long], v1: Velocity[Long], p2: Pos[Long], v2: Velocity[Long]): Option[Pos[BigDecimal]] =
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
      Some(Pos(cx, cy, 1))

  def intersectionPointWithLimits(
      minLimit: Long = 200_000_000_000_000L,
      maxLimit: Long = 400_000_000_000_000L,
    )(
      p1: Pos[Long],
      v1: Velocity[Long],
      p2: Pos[Long],
      v2: Velocity[Long],
    ): Option[Pos[BigDecimal]] =
    intersectionPoint(p1, v1, p2, v2).flatMap(pos =>
      Option.when(
        ((v1.x < 0 && pos.x < p1.x) || (v1.x > 0 && pos.x > p1.x)) &&
        ((v2.x < 0 && pos.x < p2.x) || (v2.x > 0 && pos.x > p2.x)) &&
        ((v1.y < 0 && pos.y < p1.y) || (v1.y > 0 && pos.y > p1.y)) &&
        ((v2.y < 0 && pos.y < p2.y) || (v2.y > 0 && pos.y > p2.y)) &&
        minLimit <= pos.x && pos.x <= maxLimit && minLimit <= pos.y && pos.y <= maxLimit
      )(pos)
    )

  // Thanks, Reddit - https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/keqf8uq/?utm_source=share&utm_medium=web2x&context=3
  // if the X velocities of two hailstones are the same, then the distance between them is constant.
  // So there's only a few speeds the rock can go to hit them both.
  def velocityCandidates(p1: Long, v1: Long, p2: Long, v2: Long): Option[Set[Long]] =
    if v1 == v2 then
      Some((-1000L to 1000L).foldLeft(Set.empty) { (acc, v) =>
        if v != v1 && (p2 - p1) % (v - v1) == 0 then acc + v else acc
      })
    else None

  val task1: IO[Unit] = for {
    lines: List[(Pos[Long], Velocity[Long])]                                                         <-
      Utils
        .readLines[IO]("day24.input.txt")
        .collect {
          case s"$px, $py, $pz @ $vx, $vy, $vz" =>
            Pos(px.toLong, py.toLong, pz.toLong) -> Velocity(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
        }
        .compile
        .toList
    intersection: ((Pos[Long], Velocity[Long], Pos[Long], Velocity[Long]) => Option[Pos[BigDecimal]]) =
      intersectionPointWithLimits(200_000_000_000_000L, 400_000_000_000_000L)
    res: Int                                                                                          =
      lines
        .combinations(2)
        .collect {
          case (p1: Pos[Long], v1: Velocity[Long]) :: (p2: Pos[Long], v2: Velocity[Long]) :: Nil =>
            intersection(p1, v1, p2, v2)
        }
        .flatten
        .length
    _                                                                                                <-
      IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[(Pos[Long], Velocity[Long])] <-
      Utils
        .readLines[IO]("day24.input.txt")
        .collect {
          case s"$px, $py, $pz @ $vx, $vy, $vz" =>
            Pos(px.toLong, py.toLong, pz.toLong) -> Velocity(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
        }
        .compile
        .toList
    velocityCandidatesX: List[Long]           =
      lines
        .combinations(2)
        .collect {
          case (((p1: Pos[Long]) -> (v1: Velocity[Long])) :: ((p2: Pos[Long]) -> (v2: Velocity[Long])) :: Nil) =>
            velocityCandidates(p1.x, v1.x, p2.x, v2.x)
        }
        .flatten
        .reduceLeft(_ & _)
        .toList
    velocityCandidatesY: List[Long]           =
      lines
        .combinations(2)
        .collect {
          case (((p1: Pos[Long]) -> (v1: Velocity[Long])) :: ((p2: Pos[Long]) -> (v2: Velocity[Long])) :: Nil) =>
            velocityCandidates(p1.y, v1.y, p2.y, v2.y)
        }
        .flatten
        .reduceLeft(_ & _)
        .toList
    velocityCandidatesZ: List[Long]           =
      lines
        .combinations(2)
        .collect {
          case (((p1: Pos[Long]) -> (v1: Velocity[Long])) :: ((p2: Pos[Long]) -> (v2: Velocity[Long])) :: Nil) =>
            velocityCandidates(p1.z, v1.z, p2.z, v2.z)
        }
        .flatten
        .reduceLeft(_ & _)
        .toList
    (p1: Pos[Long], v1: Velocity[Long])       = lines.head
    (p2: Pos[Long], v2: Velocity[Long])       = lines.tail.head
    rock2d: Pos[BigDecimal]                   = intersectionPoint(
                                                  p1,
                                                  v1.copy(x = v1.x - velocityCandidatesX.head, y = v1.y - velocityCandidatesY.head),
                                                  p2,
                                                  v2.copy(x = v2.x - velocityCandidatesX.head, y = v2.y - velocityCandidatesY.head),
                                                ).get
    time: Long                                = ((rock2d.x - p1.x) / (v1.x - velocityCandidatesX.head)).toLong
    rock3d: Pos[BigDecimal]                   = rock2d.copy(z = p1.z + (v1.z - velocityCandidatesZ.head) * time)
    res: BigDecimal                           = rock3d.x + rock3d.y + rock3d.z
    _                                        <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
