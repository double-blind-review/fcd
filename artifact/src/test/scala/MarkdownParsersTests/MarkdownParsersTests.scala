package fcd
package test

import markdown._

import org.scalatest._
import scala.language.higherKinds
import language.implicitConversions

class MarkdownParserTests extends FunSpec with Matchers with CustomMatchers {

  def _parsers: MarkdownParsers.type = MarkdownParsers
  override lazy val parsers = _parsers

  import parsers._

  implicit class MoreParserTestMethods[T, P <% Parser[T]](p: => P) {
    def shouldParseWith[ES <% Iterable[Elem]](s: ES, result: T) =
      it (s"""should parse "$s" with correct result""") {
        parse(p, s) shouldBe List(result)
      }
  }
  describe ("atxHeading parser") {
    atxHeading shouldParseWith (
      "## asdf #\n",
      new Heading(2, List('a', 's', 'd', 'f'))
    )

    atxHeading shouldParse "## heading \n"
  }


}
