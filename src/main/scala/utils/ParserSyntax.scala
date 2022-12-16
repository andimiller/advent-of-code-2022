package utils

import cats.parse.Parser

object ParserSyntax {

  implicit class StringParseSyntax(s: String) {
    def parseAll[A: Parser]: Either[Parser.Error, A] = implicitly[Parser[A]].parseAll(s)
  }

}
