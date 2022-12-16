package day01

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import munit.CatsEffectSuite
import utils.ParserSyntax._

class Tests extends CatsEffectSuite {
  test("parse one elf") {
    IO {
        """100
          |200""".stripMargin.parseAll[Elf]
    }.assertEquals(
      Elf(NonEmptyList.of(100L, 200L)).asRight
    )
  }

  test("parse example elves") {
    IO {
        """1000
          |2000
          |3000
          |
          |4000
          |
          |5000
          |6000
          |
          |7000
          |8000
          |9000
          |
          |10000""".stripMargin.parseAll[Elves]
    }.assertEquals(
      Elves(
        NonEmptyList.of(
          Elf(
            NonEmptyList.of(
              1000L,
              2000L,
              3000L
            )
          ),
          Elf(
            NonEmptyList.of(4000L)
          ),
          Elf(
            NonEmptyList.of(
              5000L,
              6000L
            )
          ),
          Elf(
            NonEmptyList.of(
              7000L,
              8000L,
              9000L
            )
          ),
          Elf(
            NonEmptyList.of(
              10000L
            )
          )
        )
      ).asRight
    )
  }
}
