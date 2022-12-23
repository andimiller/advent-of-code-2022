package day04

import cats.data.{Ior, NonEmptyList}
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import cats.parse.Parser
import utils.Shared
import utils.ParserSyntax.*

import scala.collection.immutable.NumericRange

type Range = NumericRange.Inclusive[Long]

case class CleaningAssignment(one: Range, two: Range):
  def hasFullContainmentOverlap: Boolean = (one.containsSlice(two) || two.containsSlice(one))
  def hasPartialOverlap: Boolean         =
    (one.contains(two.start) || one.contains(two.end) || two.contains(one.start) || two.contains(one.end))

object CleaningAssignment:
  val rangeParser: Parser[Range]   = for {
    from <- Shared.long
    _    <- Parser.char('-')
    to   <- Shared.long
  } yield from to to
  given Parser[CleaningAssignment] = (rangeParser <* Parser.char(','), rangeParser).mapN(CleaningAssignment.apply)

object PartOne extends IOApp {
  val fileParser: Parser[NonEmptyList[CleaningAssignment]] =
    implicitly[Parser[CleaningAssignment]].repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str         <- Shared.loadResource("day4.txt")
      assignments <- IO.fromEither(fileParser.parseAll(str).leftMap(e => new Throwable(e.toString)))
      count        = assignments.iterator.count(_.hasFullContainmentOverlap)
      _           <- IO.println(count)
    } yield ExitCode.Success
  }
}

object PartTwo extends IOApp {
  val fileParser: Parser[NonEmptyList[CleaningAssignment]] =
    implicitly[Parser[CleaningAssignment]].repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str         <- Shared.loadResource("day4.txt")
      assignments <- IO.fromEither(fileParser.parseAll(str).leftMap(e => new Throwable(e.toString)))
      count        = assignments.iterator.count(_.hasPartialOverlap)
      _           <- IO.println(count)
    } yield ExitCode.Success
  }
}
