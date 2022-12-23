package day05

import cats.implicits.*
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import munit.CatsEffectSuite
import utils.ParserSyntax.*

class Tests extends CatsEffectSuite {
  val newline = System.lineSeparator()
  test("transpose a grid") {
    IO {
      val input =
        """[D]
          |[N] [C]
          |[Z] [M] [P]
          | 1   2   3 """.stripMargin
      LeftTranspose.parser.parseAll(input)
    }.assertEquals(
      Right(
        " [[[" + newline + "1ZND" + newline + " ]]]" + newline + "    " + newline +
          " [[ " + newline + "2MC " + newline + " ]] " + newline + "    " + newline +
          " [  " + newline + "3P  " + newline + " ]  "
      )
    )
  }
  test("parse some stacks") {
    val input =
      """[D]
        |[N] [C]
        |[Z] [M] [P]
        | 1   2   3 """.stripMargin
    (
      for {
        transposed <- IO.fromEither(LeftTranspose.parser.parseAll(input).leftMap(e => new Throwable(e.toString)))
        stacks     <- IO.fromEither(Input.stack.parseAll(transposed).leftMap(e => new Throwable(e.toString)))
      } yield stacks
    ).assertEquals(
      NonEmptyMap.of(
        1 -> Vector('Z', 'N', 'D'),
        2 -> Vector('M', 'C'),
        3 -> Vector('P')
      )
    )
  }
  test("run the example") {
    IO {
      Input(
        NonEmptyMap.of(
          1 -> Vector('Z', 'N'),
          2 -> Vector('M', 'C', 'D'),
          3 -> Vector('P')
        ),
        NonEmptyList.of(
          MoveCommand(1, 2, 1),
          MoveCommand(3, 1, 3),
          MoveCommand(2, 2, 1),
          MoveCommand(1, 1, 2)
        )
      ).run
    }.assertEquals(
      NonEmptyMap.of(
        1 -> Vector('C'),
        2 -> Vector('M'),
        3 -> Vector('P', 'D', 'N', 'Z')
      )
    )
  }

}
