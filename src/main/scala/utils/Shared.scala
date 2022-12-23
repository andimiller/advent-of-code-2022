package utils

import cats.effect.IO
import cats.parse.{Parser, Parser0}
import fs2.io.file.{Files, Path}

import java.nio.file.Path as JPath

object Shared {

  val int: Parser[Int]              = Parser.charsWhile(_.isDigit).mapFilter(_.toIntOption)
  val long: Parser[Long]            = Parser.charsWhile(_.isDigit).mapFilter(_.toLongOption)
  val newline: Parser[Unit]         = Parser.string(System.lineSeparator())
  val doubleNewline: Parser[Unit]   = Parser.string(System.lineSeparator() + System.lineSeparator())
  val skipWhitespace: Parser0[Unit] = Parser.charsWhile(Set(' ', '\t')).rep0.void

  def loadResource(path: String): IO[String] =
    Files[IO]
      .readAll(Path.fromNioPath(JPath.of("src", "main", "resources", path)))
      .through(fs2.text.utf8.decode[IO])
      .compile
      .string

}
