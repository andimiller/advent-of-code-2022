package day05

import cats.data.{Ior, NonEmptyList, NonEmptyMap}
import cats.effect.{ExitCode, IO, IOApp}
import cats.parse.Parser
import cats.implicits.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.Size
import utils.Shared
import utils.RefinedInstances.*

import scala.collection.immutable.SortedMap

object LeftTranspose:
  val parser: Parser[String] = Parser.charWhere(!Set('\r', 'n').contains(_)).rep.repSep(Shared.newline).map { grid =>
    val longestLine = grid.map(_.size).toList.max
    grid.toList.map(_.toList.padTo(longestLine, ' ')).transpose.map(_.reverse.mkString).mkString(System.lineSeparator())
  }

case class MoveCommand(count: Int, from: Int, to: Int)
object MoveCommand:
  val parser: Parser[MoveCommand] = for {
    _     <- Parser.string("move ")
    count <- Shared.int
    _     <- Parser.string(" from ")
    from  <- Shared.int
    _     <- Parser.string(" to ")
    to    <- Shared.int
  } yield MoveCommand(count, from, to)

case class Input(
    stacks: NonEmptyMap[Int, Vector[Char]],
    commands: NonEmptyList[MoveCommand]
):
  def run: NonEmptyMap[Int, Vector[Char]] = commands.foldLeft(stacks) { case (s, c) =>
    val moveme = s(c.from).get.takeRight(c.count)
    s
      .updateWith(c.from)(_.dropRight(c.count))
      .updateWith(c.to)(_ ++ moveme.reverse)
  }

  def run2: NonEmptyMap[Int, Vector[Char]] = commands.foldLeft(stacks) { case (s, c) =>
    val moveme = s(c.from).get.takeRight(c.count)
    s
      .updateWith(c.from)(_.dropRight(c.count))
      .updateWith(c.to)(_ ++ moveme)
  }
object Input:
  val emptyLine                                     = Parser.charsWhile(Set(' ', '[', ']')).as(none[(Int, Vector[Char])])
  val stackLine                                     = for {
    index <- Shared.int
    chars <- Parser.charWhere(('A' to 'Z').toSet).rep.map(_.toList.toVector) <* Shared.skipWhitespace
  } yield Some(index, chars)
  val stack: Parser[NonEmptyMap[Int, Vector[Char]]] = (emptyLine.orElse(stackLine)).repSep(Shared.newline).map { nel =>
    NonEmptyMap.fromMapUnsafe(SortedMap.from(nel.toList.flatten.toMap))
  }
  val splitHalves: Parser[(String, String)]         =
    (Parser.anyChar.repUntil(Shared.doubleNewline).string <* Shared.doubleNewline, Parser.anyChar.rep.string).tupled
  val commands: Parser[NonEmptyList[MoveCommand]]   = MoveCommand.parser.repSep(Shared.newline)

object PartOne extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str        <- Shared.loadResource("day5.txt")
      split      <- IO.fromEither(Input.splitHalves.parseAll(str).leftMap(e => new Throwable(e.toString)))
      transposed <- IO.fromEither(LeftTranspose.parser.parseAll(split._1).leftMap(e => new Throwable(e.toString)))
      stacks     <- IO.fromEither(Input.stack.parseAll(transposed).leftMap(e => new Throwable(e.toString)))
      commands   <- IO.fromEither(Input.commands.parseAll(split._2).leftMap(e => new Throwable(e.toString)))
      input       = Input(stacks, commands)
      result      = input.run
      top         = result.toNel.map(_._2.last).toList.mkString
      _          <- IO.println(top)
    } yield ExitCode.Success
  }
}

object PartTwo extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str        <- Shared.loadResource("day5.txt")
      split      <- IO.fromEither(Input.splitHalves.parseAll(str).leftMap(e => new Throwable(e.toString)))
      transposed <- IO.fromEither(LeftTranspose.parser.parseAll(split._1).leftMap(e => new Throwable(e.toString)))
      stacks     <- IO.fromEither(Input.stack.parseAll(transposed).leftMap(e => new Throwable(e.toString)))
      commands   <- IO.fromEither(Input.commands.parseAll(split._2).leftMap(e => new Throwable(e.toString)))
      input       = Input(stacks, commands)
      result      = input.run2
      top         = result.toNel.map(_._2.last).toList.mkString
      _          <- IO.println(top)
    } yield ExitCode.Success
  }
}
