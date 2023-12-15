package se.yankov.aoc2023

import java.net.URL

import cats.Monad
import cats.effect.{ Async, IO }
import cats.syntax.all.*
import fs2.*
import fs2.{ Chunk, Stream }
import fs2.io.file.{ Files, Flags, Path }
import scala.io.Source

object Utils {

  def readLines[F[_]: Async](fileName: String): Stream[F, String] =
    val path: String = s"src/main/resources/$fileName"
    Files
      .forAsync[F]
      .readAll(Path(path))
      .through(text.utf8.decode)
      .through(text.lines)

  def readCSVLine[F[_]: Async](fileName: String): Stream[F, String] =
    val path: String = s"src/main/resources/$fileName"
    Files
      .forAsync[F]
      .readAll(Path(path))
      .through(text.utf8.decode)
      .repartition((s: String) => Chunk.array(s.split(",", -1)))
      .dropLastIf(_.isEmpty)

  extension [A](io: IO[A])
    def dbg: IO[A] = io.flatTap { r =>
      IO.println(s"${Thread.currentThread().getName}: $r")
    }

}
