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
  // ###########################################################################
  // ######################## ATX Heading Tetsts ###############################
// ###########################################################################
  describe ("Simple headings:") {
    atxHeading shouldParseWith (
      "# foo\n",
      (1, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "## foo\n",
      (2, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "### foo\n",
      (3, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "#### foo\n",
      (4, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "##### foo\n",
      (5, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "###### foo\n",
      (6, List('f','o','o'))
    )
  }
  describe ("More than six # characters is not a heading:") {
    atxHeading shouldNotParse ("####### foo\n")
  }
  describe ("At least one space is required between the # characters and the heading’s contents, unless the heading is empty:") {
    atxHeading shouldNotParse ("#5 bolt\n")
    atxHeading shouldNotParse ("#hashtag\n")
  }
  describe ("This is not a heading, because the first # is escaped:") {
    atxHeading shouldNotParse ("\\## foo\n")
  }
  describe ("Contents are parsed as inlines:") {
    atxHeading shouldParseWith (
      "# foo *bar* \\*baz\\*\n",
      (1, List('f','o','o', ' ', '*', 'b', 'a', 'r','*', ' ' , '\\', '*', 'b', 'a', 'z','\\', '*' ))
    )
  }
  describe ("Leading and trailing blanks are ignored in parsing inline content:") {
    atxHeading shouldParseWith (
      "#                  foo                     \n",
      (1, List('f','o','o'))
    )
  }
  describe ("One to three spaces indentation are allowed:") {
    atxHeading shouldParseWith (
      " ### foo\n",
      (3, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "  ## foo\n",
      (2, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "   # foo\n",
      (1, List('f','o','o'))
    )
  }
  describe ("Four spaces are too much:") {
    atxHeading shouldNotParse ("    # foo\n")
  }
  describe ("A closing sequence of # characters is optional:") {
    atxHeading shouldParseWith (
      "## foo ##\n",
      (2, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "  ###   bar    ###\n",
      (3, List('b','a','r'))
    )
  }
  describe ("It need not be the same length as the opening sequence:") {
    atxHeading shouldParseWith (
      "# foo ##################################\n",
      (1, List('f','o','o'))
    )
    atxHeading shouldParseWith (
      "##### foo ##\n",
      (5, List('f','o','o'))
    )
  }
  describe ("Spaces are allowed after the closing sequence:") {
    atxHeading shouldParseWith (
      "### foo ###     \n",
      (3, List('f','o','o'))
    )
  }
  // Abweichend von der CommonMarkSpec:
  describe ("Characters are not allowed in the closing sequence:") {
    atxHeading shouldNotParse ("### foo ### b\n")
  }
  describe ("The closing sequence must be preceded by a space:") {
    atxHeading shouldNotParse ("# foo#\n")
  }
  describe ("ATX headings can be empty:") {
    atxHeading shouldParseWith (
      "## \n",
      (2, List())
    )
    atxHeading shouldParseWith (
      "# \n",
      (1, List())
    )
  }


}
