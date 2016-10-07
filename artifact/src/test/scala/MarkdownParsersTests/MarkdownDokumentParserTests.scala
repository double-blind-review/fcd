package fcd
package test

import markdown._

import org.scalatest._
import scala.language.higherKinds
import language.implicitConversions

class MarkdownDocumentParserTests extends FunSpec with Matchers with CustomMatchers {

  def _parsers: MarkdownParsers.type = MarkdownParsers
  override lazy val parsers = _parsers

  import parsers._

  implicit class MoreParserTestMethods[T, P <% Parser[T]](p: => P) {
    def shouldParseWith[ES <% Iterable[Elem]](s: ES, result: T) =
      it (s"""should parse "$s" with correct result""") {
        parse(p, s) shouldBe List(result)
      }
  }

  describe("the parser should parse a heading or a paragraph") {
    mdAtxParagraph shouldParseWith(
      "p\n\u0000",
      List(Paragraph("p\n"))
    )
    mdAtxParagraph shouldParseWith(
      "# h\n\u0000",
      List(Heading(1, "h"))
    )
  }
  describe("the parser shoud greedy read a paragraph") {
    mdAtxParagraph shouldParseWith(
      "p\nt\n\u0000",
      List(Paragraph("p\nt\n"))
    )
    mdAtxParagraph shouldParseWith(
      "p\na\n# h\n\u0000",
      List(Paragraph("p\na\n"), Heading(1,"h"))
    )
  }
  describe("the parser shoud read a atxHeading bevor a paragraph") {
    mdAtxParagraph shouldParseWith(
      "# h\np\n\u0000",
      List(Heading(1, "h"),Paragraph("p\n"))
    )
  }
  describe("the parser shoud get broaken by an ATX heading") {
    mdAtxParagraph shouldParseWith(
      "p\n# h\n\u0000",
      List(Paragraph("p\n"),Heading(1, "h"))
    )
  }
  describe("a paragraph should can be surrounded by empty lines") {
    mdEmptyParagraph shouldParseWith(
      "p\n\n\u0000",
      List(Paragraph("p\n"), EmptyLine())
    )
    mdEmptyParagraph shouldParseWith(
      "\np\n\u0000",
      List(EmptyLine(), Paragraph("p\n"))
    )
  }
  describe("a paragraph can be inside of a blockQuote") {
    mdBlockParagraph shouldParseWith(
      "> p\n\u0000",
      List(BlockQuote(List(Paragraph("p\n"))))
    )
  }
  describe("a blockQuote can be inside of a blockQuote") {
    mdBlockParagraph shouldParseWith(
      "> > p\n\u0000",
      List(BlockQuote(List(BlockQuote(List(Paragraph("p\n"))))))
    )
  }
  describe("a blockQuote can be multiline") {
    mdBlockParagraph shouldParseWith(
      "> p\n> p\n\u0000",
      List(BlockQuote(List(Paragraph("p\np\n"))))
    )
  }
  describe("a setextHeading is not a paragraph") {
    mdSetextParagraph shouldParseWith(
      "s\n---\n\u0000",
      List(Heading(2,"s\n"))
    )
  }
  describe("a setextHeading is not a paragraph but can followed by a paragraph") {
    mdSetextParagraph shouldParseWith(
      "s\n---\np\n\u0000",
      List(Heading(2,"s\n"), Paragraph("p\n"))
    )
  }
}
