package utils

import cats.effect.IO
import cats.parse.Parser
import fs2.io.file.{Files, Path}
import java.nio.file.{Path => JPath}

object Shared {

  val long: Parser[Long]    = Parser.charsWhile(_.isDigit).mapFilter(_.toLongOption)
  val newline: Parser[Unit] = Parser.string(System.lineSeparator())

  def loadResource(path: String): IO[String] =
    Files[IO]
      .readAll(Path.fromNioPath(JPath.of("src", "main", "resources", path)))
      .through(fs2.text.utf8.decode[IO])
      .compile
      .string

}
