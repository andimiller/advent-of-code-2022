package day02

import cats.data.{Ior, NonEmptyList}
import cats.effect.{ExitCode, IO, IOApp}
import cats.parse.Parser
import cats.implicits.*
import utils.Shared

object Action:
  val parser: Parser[Action] =
    Parser.oneOf(
      List(
        Parser.char('A').as(Rock),
        Parser.char('B').as(Paper),
        Parser.char('C').as(Scissors),
        Parser.char('X').as(Rock),
        Parser.char('Y').as(Paper),
        Parser.char('Z').as(Scissors)
      )
    )

enum Action:
  case Rock, Paper, Scissors

import Action._
extension (left: Action)
  def score                            = left match
    case Action.Rock     => 1
    case Action.Paper    => 2
    case Action.Scissors => 3

  def vs(right: Action): Ior[Int, Int] = (left, right) match
    case (l, r) if l == r  => Ior.Both(3, 3)
    case (Rock, Scissors)  => Ior.Left(6)
    case (Paper, Rock)     => Ior.Left(6)
    case (Scissors, Paper) => Ior.Left(6)
    case (Rock, Paper)     => Ior.Right(6)
    case (Paper, Scissors) => Ior.Right(6)
    case (Scissors, Rock)  => Ior.Right(6)

case class Line(left: Action, right: Action):
  def score = NonEmptyList
    .of(
      Ior.Left(left.score),
      Ior.Right(right.score),
      left vs right
    )
    .reduce

object Line:
  val parser: Parser[Line] = for {
    left  <- Action.parser
    _     <- Parser.char(' ')
    right <- Action.parser
  } yield Line(left, right)

object PartOne extends IOApp {
  val file: Parser[NonEmptyList[Line]] = Line.parser.repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      s      <- Shared.loadResource("day2.txt")
      inputs <- IO.fromEither(file.parseAll(s).leftMap(e => new Throwable(e.toString())))
      score   = inputs.map(_.score).reduce
      _      <- IO.println(s"final score: $score")
    } yield ExitCode.Success
}

// Part Two

enum Outcome:
  case Lose, Draw, Win
import Outcome._

object Outcome:
  val parser: Parser[Outcome] = Parser.oneOf(
    List(
      Parser.char('X').as(Lose),
      Parser.char('Y').as(Draw),
      Parser.char('Z').as(Win)
    )
  )

case class Line2(left: Action, outcome: Outcome):
  def right: Action = outcome match
    case Draw => left
    case Lose =>
      left match
        case Rock     => Scissors
        case Paper    => Rock
        case Scissors => Paper
    case Win  =>
      left match
        case Rock     => Paper
        case Paper    => Scissors
        case Scissors => Rock

  def resolve: Line = Line(left, right)

object Line2:
  val parser: Parser[Line2] =
    (Action.parser <* Parser.char(' '), Outcome.parser).mapN(Line2)

object PartTwo extends IOApp {
  val file: Parser[NonEmptyList[Line2]] = Line2.parser.repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      s      <- Shared.loadResource("day2.txt")
      inputs <- IO.fromEither(file.parseAll(s).leftMap(e => new Throwable(e.toString())))
      score   = inputs.map(_.resolve.score).reduce
      _      <- IO.println(s"final score: $score")
    } yield ExitCode.Success
}
