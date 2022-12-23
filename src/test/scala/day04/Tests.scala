package day04

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import munit.CatsEffectSuite
import utils.ParserSyntax.*

class Tests extends CatsEffectSuite {
  test("parse a CleaningAssignment") {
    IO {
      "1-5,6-10".parseAll[CleaningAssignment]
    }.assertEquals(
      CleaningAssignment(1L to 5L, 6L to 10L).asRight
    )
  }

}
