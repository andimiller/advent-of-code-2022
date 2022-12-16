package day03

import cats.data.{Ior, NonEmptyList}
import cats.effect.{ExitCode, IO, IOApp}
import cats.parse.{Parser, Parser0}
import cats.implicits.*
import cats.kernel.Order
import utils.Shared
import eu.timepit.refined.api.{Refined, Validate, Result}
import eu.timepit.refined.refineV
import eu.timepit.refined.collection.*
import eu.timepit.refined.auto.*

lazy val priority: Map[Char, Int] = (('a' to 'z') ++ ('A' to 'Z')).zip(LazyList.from(1)).toMap
given Ordering[Char]              = Ordering.by(priority)

case class Rucksack(firstCompartment: Set[Char], secondCompartment: Set[Char]):
  def all: Set[Char]     = firstCompartment ++ secondCompartment
  def overlap: Set[Char] = firstCompartment.intersect(secondCompartment)
  def score: Int         = overlap.iterator.map(priority).sum

object Rucksack:
  val parser: Parser[Rucksack] = Parser.charIn(priority.keySet).rep.filter(_.size % 2 == 0).map { chars =>
    val halfLength = chars.size / 2
    Rucksack(chars.take(halfLength).toSet, chars.reverse.take(halfLength).toSet)
  }

object PartOne extends IOApp {
  val file: Parser[NonEmptyList[Rucksack]] = Rucksack.parser.repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str       <- Shared.loadResource("day3.txt")
      rucksacks <- IO.fromEither(file.parseAll(str).leftMap(s => new Throwable(s.toString)))
      score      = rucksacks.map(_.score).reduce
      _         <- IO.println(score)
    } yield ExitCode.Success
  }
}

object PartTwo extends IOApp {
  type ElfGroup = NonEmptyList[Rucksack] Refined Size[3]
  given [T, N <: Int: ValueOf]: Validate[NonEmptyList[T], Size[N]] =
    Validate.instance({ nel => Result.fromBoolean(nel.size == valueOf[N], nel) }, _.toString)

  val file: Parser[NonEmptyList[ElfGroup]] =
    Rucksack.parser
      .repSep(3, 3, Shared.newline)
      .map(nel => refineV[Size[3]].unsafeFrom(nel))
      .repSep(Shared.newline)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str    <- Shared.loadResource("day3.txt")
      groups <- IO.fromEither(file.parseAll(str).leftMap(s => new Throwable(s.toString)))
      result  = groups.map { elves =>
                  elves.map(_.all).reduceLeft(_ intersect _).iterator.map(priority.apply).sum
                }.reduce
      _      <- IO.println(result)
    } yield ExitCode.Success
  }
}
