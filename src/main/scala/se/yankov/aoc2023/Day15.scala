package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }

object Day15 extends IOApp.Simple {

  enum Operation(val label: String, val boxId: Int):
    case Dash(override val label: String, override val boxId: Int)              extends Operation(label, boxId)
    case Equal(override val label: String, value: Int, override val boxId: Int) extends Operation(label, boxId)

  final case class Lens(label: String, focalLength: Int)

  final case class Box(lenses: Vector[Lens]):
    def removeLens(label: String): Box                  = copy(lenses = lenses.filterNot(_.label == label))
    def addOrUpdateLens(label: String, value: Int): Box =
      val index: Int = lenses.indexWhere(_.label == label)
      if index > -1 then copy(lenses = lenses.updateAndSwap(index, _ => Lens(label, value)))
      else copy(lenses = lenses :+ Lens(label, value))

  extension (s: String) def hash: Int = s.foldLeft(0)((acc: Int, c: Char) => (17 * (acc + c.toInt)) % 256)

  extension [A](v: Vector[A]) def updateAndSwap(index: Int, f: A => A): Vector[A] = v.updated(index, f(v(index)))

  extension (boxes: Vector[Box])
    def focusingPower: Int =
      boxes
        .zipWithIndex
        .map((box, i) => box.lenses.zipWithIndex.map((lens, j) => (i + 1) * (j + 1) * lens.focalLength).sum)
        .sum

  extension (operations: Vector[Operation])
    def apply(boxes: Vector[Box]): Vector[Box] = operations.foldLeft(boxes) {
      case (acc: Vector[Box], Operation.Dash(label, boxId))         =>
        acc.updateAndSwap(boxId, _.removeLens(label))
      case (acc: Vector[Box], Operation.Equal(label, value, boxId)) =>
        acc.updateAndSwap(boxId, _.addOrUpdateLens(label, value))
    }

  val task1: IO[Unit] = Utils.readCSVLine[IO]("day15.input.txt").map(_.hash).reduce(_ + _).printlns.compile.drain

  val task2: IO[Unit] = for {
    operations: Vector[Operation] <- Utils
                                       .readCSVLine[IO]("day15.input.txt")
                                       .collect {
                                         case s"$label=$value" => Operation.Equal(label, value.toInt, label.hash)
                                         case s"$label-"       => Operation.Dash(label, label.hash)
                                       }
                                       .compile
                                       .toVector
    boxesInitialState: Vector[Box] = Vector.fill(256)(Box(Vector.empty))
    res: Int                       = operations.apply(boxesInitialState).focusingPower
    _                             <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
