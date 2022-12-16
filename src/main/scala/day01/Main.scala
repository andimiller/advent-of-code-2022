package day01

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.parse._
import cats.implicits._
import utils._
import ParserSyntax._
import fs2.Stream

case class Elf(food: NonEmptyList[Long]) {
  def sum: Long = food.sumAll
}
object Elf                               {
  implicit val parser: Parser[Elf] = Shared.long.repSep(Shared.newline).map(Elf(_))
}
case class Elves(elves: NonEmptyList[Elf])
object Elves                             {
  implicit val parser: Parser[Elves] =
    Elf.parser.repSep(Shared.newline.rep(2)).map(Elves(_)) <* Shared.newline.rep0.void
}

object PartOne extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str   <- Shared.loadResource("day1.txt")
      elves <- IO.fromEither(str.parseAll[Elves].leftMap(e => new Throwable(e.toString())))
      winner = elves.elves.maximumBy(_.sum)
      _     <- IO.println(winner.sum)
    } yield ExitCode.Success
  }
}

object PartTwo extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str    <- Shared.loadResource("day1.txt")
      elves  <- IO.fromEither(str.parseAll[Elves].leftMap(e => new Throwable(e.toString())))
      winners = elves.elves.map(_.sum).sorted.reverse.take(3)
      _      <- IO.println(winners.sum)
    } yield ExitCode.Success
  }
}
