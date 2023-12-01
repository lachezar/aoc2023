package se.yankov.aoc2023

import java.net.URL

import cats.Monad
import cats.effect.{ Async, IO }
import cats.syntax.all.*
import fs2.*
import fs2.Stream
import fs2.io.file.{ Files, Flags, Path }
import scala.io.Source

object Utils {

  def readLines[F[_]: Async](fileName: String): Stream[F, String] =
    val path: String = s"src/main/resources/$fileName"
    Files
      .forAsync[F]
      .readAll(Path(path), 4096, Flags.Read)
      .through(text.utf8.decode)
      .through(text.lines)

  extension [A](io: IO[A])
    def dbg: IO[A] = io.flatTap { r =>
      IO.println(s"${Thread.currentThread().getName}: $r")
    }

}
