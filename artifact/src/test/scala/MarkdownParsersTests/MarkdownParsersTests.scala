package fcd
package test

import markdown._

import org.scalatest._
import scala.language.higherKinds
import language.implicitConversions

class MarkdownParserTests extends FunSpec with Matchers with CustomMatchers {

  val blockQuoteTests = false;
  val documentParser = false;

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
  def H1(content: String) =
    Heading(1, content)
  def H2(content: String) =
    Heading(2, content)
  def H3(content: String) =
    Heading(3, content)
  def H4(content: String) =
    Heading(4, content)
  def H5(content: String) =
    Heading(5, content)
  def H6(content: String) =
    Heading(6, content)

  describe ("Simple headings:") {
    atxHeading shouldParseWith (
      "# foo\n",
      H1("foo")
    )
    atxHeading shouldParseWith (
      "## foo\n",
      H2("foo")
    )
    atxHeading shouldParseWith (
      "### foo\n",
      H3("foo")
    )
    atxHeading shouldParseWith (
      "#### foo\n",
      H4("foo")
    )
    atxHeading shouldParseWith (
      "##### foo\n",
      H5("foo")
    )
    atxHeading shouldParseWith (
      "###### foo\n",
      H6("foo")
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
      H1("foo *bar* \\*baz\\*")
    )
  }
  describe ("Leading and trailing blanks are ignored in parsing inline content:") {
    atxHeading shouldParseWith (
      "#                  foo                     \n",
      H1("foo")
    )
  }
  describe ("One to three spaces indentation are allowed:") {
    atxHeading shouldParseWith (
      " ### foo\n",
      H3("foo")
    )
    atxHeading shouldParseWith (
      "  ## foo\n",
      H2("foo")
    )
    atxHeading shouldParseWith (
      "   # foo\n",
      H1("foo")
    )
  }
  if(documentParser){
    describe ("Four spaces are too much:") {
      mdIndetedAtx shouldParseWith(
        "    # foo\n\u0000",
        List(CodeBlock("# foo\n"))
      )
    }
  }
  describe ("A closing sequence of # characters is optional:") {
    atxHeading shouldParseWith (
      "## foo ##\n",
      H2("foo")
    )
    atxHeading shouldParseWith (
      "  ###   bar    ###\n",
      H3("bar")
    )
  }
  describe ("It need not be the same length as the opening sequence:") {
    atxHeading shouldParseWith (
      "# foo ##################################\n",
      H1("foo")
    )
    atxHeading shouldParseWith (
      "##### foo ##\n",
      H5("foo")
    )
  }
  describe ("Spaces are allowed after the closing sequence:") {
    atxHeading shouldParseWith (
      "### foo ###     \n",
      H3("foo")
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
      H2("")
    )
    atxHeading shouldParseWith (
      "# \n",
      H1("")
    )
  }
  describe("ATX headings should not go over 2 or more lines"){
    atxHeading shouldNotParse ("# heading \ntest \n")
  }
  // ###########################################################################
  // #################### Indented Code Block Tests ############################
  // ###########################################################################
  describe ("a simple Code Block parser") {
    indentedCodeBlock shouldParseWith  (
      "    a simple\n      indented code block\n",
      CodeBlock("a simple\n  indented code block\n")
    )
  }
  describe ("The contents of a code block are literal text, and do not get parsed as Markdown:") {
    indentedCodeBlock shouldParseWith  (
      "    <a/>\n    *hi*\n\n    - one \n",
      CodeBlock("<a/>\n*hi*\n\n- one \n")
    )
  }
  describe ("Here we have three chunks separated by blank lines:") {
    indentedCodeBlock shouldParseWith  (
      "    chunk1\n\n    chunk2\n  \n \n \n    chunk3 \n",
      CodeBlock("chunk1\n\nchunk2\n\n\n\nchunk3 \n")
    )
  }
  describe ("Any initial spaces beyond four will be included in the content, even in interior blank lines:") {
    indentedCodeBlock shouldParseWith  (
      "    chunk1\n      \n      chunk2\n",
      CodeBlock("chunk1\n  \n  chunk2\n")
    )
  }
  describe ("Trailing spaces are included in the code block’s content:") {
    indentedCodeBlock shouldParseWith  (
      "    foo  \n",
      CodeBlock("foo  \n")
    )
  }
  describe ("The first line can be indented more than four spaces:") {
    indentedCodeBlock shouldParseWith  (
      "        foo\n    bar\n",
      CodeBlock("    foo\nbar\n")
    )
  }
  // ###########################################################################
  // ##################### Fenced Code Block Tests #############################
  // ###########################################################################
  describe ("Here is a simple example with backticks:") {
    fencedCodeBlock shouldParseWith  (
      "```\n<\n >\n```\n",
      CodeBlock("<\n >\n")
    )
  }
  describe ("Here is a simple example with tildes:") {
    fencedCodeBlock shouldParseWith  (
      "~~~\n<\n >\n~~~\n",
        CodeBlock("<\n >\n")
    )
  }
  describe ("The closing code fence must use the same character as the opening fence:") {
    fencedCodeBlock shouldParseWith  (
      "```\naaa\n~~~\n```\n",
        CodeBlock("aaa\n~~~\n")
    )
    fencedCodeBlock shouldParseWith  (
      "~~~\naaa\n```\n~~~\n",
      CodeBlock("aaa\n```\n")
    )
  }
  describe ("The closing code fence must be at least as long as the opening fence:") {
    fencedCodeBlock shouldParseWith  (
      "````\naaa\n```\n``````\n",
      CodeBlock("aaa\n```\n")
    )
    fencedCodeBlock shouldParseWith  (
      "~~~~\naaa\n~~~\n~~~~\n",
      CodeBlock("aaa\n~~~\n")
    )
  }
  describe ("Unclosed code blocks are closed by the end of the document:") {
    fencedCodeBlock shouldParseWith  (
      "```\n" + 0.toChar,
      CodeBlock("")
    )
    fencedCodeBlock shouldParseWith  (
      "`````\n\n```\naaa\n" + 0.toChar,
      CodeBlock("\n```\naaa\n")
    )
  }
  describe ("A code block can have all empty lines as its content:") {
    fencedCodeBlock shouldParseWith  (
      "```\n\n  \n```\n",
      CodeBlock("\n  \n")
    )
  }
  describe ("Fences can be indented. If the opening fence is indented, content lines will have equivalent opening indentation removed, if present:") {
    fencedCodeBlock shouldParseWith  (
      " ```\n aaa\naaa\n```\n",
      CodeBlock("aaa\naaa\n")
    )
    fencedCodeBlock shouldParseWith  (
      "  ```\naaa\n  aaa\naaa\n  ```\n",
      CodeBlock("aaa\naaa\naaa\n")
    )
    fencedCodeBlock shouldParseWith  (
      "   ```\n   aaa\n    aaa\n  aaa\n   ```\n",
      CodeBlock("aaa\n aaa\naaa\n")
    )
  }
  if(documentParser){
    describe ("Four spaces indentation produces an indented code block:") {
      mdIndetedFenced shouldParseWith  (
        "    ```\n    aaaa\n    ```\n\u0000",
        List(CodeBlock("```\naaa\n```\n"))
      )
    }
  }

  describe ("Closing fences may be indented by 0-3 spaces, and their indentation need not match that of the opening fence:") {
    fencedCodeBlock shouldParseWith  (
      "```\naaa\n  ```\n",
      CodeBlock("aaa\n")
    )
    fencedCodeBlock shouldParseWith  (
      "   ```\naaa\n  ```\n",
      CodeBlock("aaa\n")
    )
  }
  describe ("This is not a closing fence, because it is indented 4 spaces:") {
    fencedCodeBlock shouldParseWith  (
      "```\naaa\n    ```\n" + 0.toChar,
      CodeBlock("aaa\n    ```\n")
    )
  }
  describe ("Code fences (opening and closing) cannot contain internal spaces:") {
    fencedCodeBlock shouldNotParse  (
      "``` ```\naaa\n"
    )
    fencedCodeBlock shouldParseWith  (
      "~~~~~~\naaa\n~~~ ~~\n" + 0.toChar,
      CodeBlock("aaa\n~~~ ~~\n")
    )
  }

  // Infostrings werden nicht implementiert!

  // ###########################################################################
  // ####################### Thematic Breaks Tests #############################
  // ###########################################################################
  describe ("A line consisting of 0-3 spaces of indentation, followed by a sequence of three or more matching -, _, or * characters, each followed optionally by any number of spaces, forms a thematic break:") {
    thematicBreak shouldParse  ("***\n")
    thematicBreak shouldParse  ("---\n")
    thematicBreak shouldParse  ("___\n")
  }
  describe ("Wrong characters:") {
    thematicBreak shouldNotParse  ("+++\n")
    thematicBreak shouldNotParse  ("===\n")
  }
  describe ("Not enough characters:") {
    thematicBreak shouldNotParse  ("--\n")
    thematicBreak shouldNotParse  ("**\n")
    thematicBreak shouldNotParse  ("__\n")
  }
  describe ("One to three spaces indent are allowed:") {
    thematicBreak shouldParse  (" ***\n")
    thematicBreak shouldParse  ("  ***\n")
    thematicBreak shouldParse  ("   ***\n")
  }
  if(documentParser){
    describe ("Four spaces is too many:") {
      mdIndetedThematic shouldParseWith  (
        "    ***\n\u0000",
        List(ThematicBreak())
      )
    }
  }

  describe ("More than three characters may be used:") {
    thematicBreak shouldParse  ("_____________________________________\n")
  }
  describe ("Spaces are allowed between the characters:") {
    thematicBreak shouldParse  (" - - -\n")
    thematicBreak shouldParse  (" **  * ** * ** * **\n")
    thematicBreak shouldParse  ("-     -      -      -\n")
  }
  describe ("Spaces are allowed at the end:") {
    thematicBreak shouldParse  ("- - - -    \n")
  }
  describe ("However, no other characters may occur in the line:") {
    thematicBreak shouldNotParse  ("_ _ _ _ a\n")
    thematicBreak shouldNotParse  ("a------\n")
    thematicBreak shouldNotParse  ("---a---\n")
  }
  describe ("It is required that all of the non-whitespace characters be the same. So, this is not a thematic break:") {
    thematicBreak shouldNotParse  (" *-*\n")
  }

  // ###########################################################################
  // ######################## Setext Heading Tests #############################
  // ###########################################################################
  describe ("Simple examples:") {
    setextHeading shouldParseWith  (
      "Foo *bar*\n=========\n",
      H1("Foo *bar*\n")
    )
    setextHeading shouldParseWith  (
      "Foo *bar*\n---------\n",
      H2("Foo *bar*\n")
    )
  }
  describe ("Simple examples:") {
    setextHeading shouldParseWith  (
      "Foo *bar\nbaz*\n=========\n",
      H1("Foo *bar\nbaz*\n")
    )
  }
  describe ("The underlining can be any length:") {
    setextHeading shouldParseWith  (
      "Foo\n-------------------------\n",
      H2("Foo\n")
    )
    setextHeading shouldParseWith  (
      "Foo\n=\n",
      H1("Foo\n")
    )
  }
  describe ("The heading content can be indented up to three spaces, and need not line up with the underlining:") {
    setextHeading shouldParseWith  (
      "   Foo\n---\n",
      H2("Foo\n")
    )
    setextHeading shouldParseWith  (
      "  Foo\n-----\n",
      H2("Foo\n")
    )
    setextHeading shouldParseWith  (
      "  Foo\n  ==\n",
      H1("Foo\n")
    )
  }
  describe ("Four spaces indent is too much:") {
    if(documentParser){
      mdIndetedSetext shouldParseWith  (
        "    Foo\n    ---\n\u0000",
        List(CodeBlock("Foo\n---\n"))
      )
    }
    setextHeading shouldNotParse  (
      "    Foo\n----\n"
    )
  }
  describe ("The setext heading underline can be indented up to three spaces, and may have trailing spaces:") {
    setextHeading shouldParseWith  (
      "Foo\n   ----      \n",
      H2("Foo\n")
    )
  }
  describe ("Four spaces is too much:") {
    setextHeading shouldNotParse (
      "Foo\n    ----\n"
    )
  }
  describe ("The setext heading underline cannot contain internal spaces:") {
    setextHeading shouldNotParse (
      "Foo\n= =\n"
    )
    setextHeading shouldNotParse (
      "Foo\n--- -\n"
    )
  }

  // ###########################################################################
  // ########################## Paragraphs Tests ###############################
  // ###########################################################################
  describe ("A simple example with two paragraphs:") {
    paragraph shouldParseWith (
      "aaa\n",
      Paragraph("aaa\n")
    )
  }
  describe ("Paragraphs can contain multiple lines, but no blank lines:") {
    paragraph shouldNotParse (
      "aaa\nbbb\n\n"
    )
    paragraph shouldParseWith (
      "ccc\nddd\n",
      Paragraph("ccc\nddd\n")
    )
  }
  describe ("Leading spaces are skipped:") {
    paragraph shouldParseWith (
      "  aaa\n bbb\n",
      Paragraph("aaa\nbbb\n")
    )
  }
  describe ("Lines after the first may be indented any amount, since indented code blocks cannot interrupt paragraphs.") {
    paragraph shouldParseWith (
      "aaa\n             bbb\n                                       ccc\n",
      Paragraph("aaa\nbbb\nccc\n")
    )

  }
  describe ("However, the first line may be indented at most three spaces, or an indented code block will be triggered:") {
    paragraph shouldParseWith (
      "   aaa\nbbb\n",
      Paragraph("aaa\nbbb\n")
    )
    paragraph shouldNotParse (
      "    aaa\nbbb\n"
    )
  }
  // ###########################################################################
  // ####################### Block Detection Tests #############################
  // ###########################################################################
  if(blockQuoteTests){
    describe("the parser should parse a heading or a paragraph") {
      mdAtxParagraph shouldParseWith(
        "paragraph\n\u0000",
        List(Paragraph("paragraph\n"))
      )
      mdAtxParagraph shouldParseWith(
        "# heading\n\u0000",
        List(Heading(1, "heading"))
      )
    }
    describe("the parser shoud greedy read a paragraph") {
      mdAtxParagraph shouldParseWith(
        "paragraph\ntest\n\u0000",
        List(Paragraph("paragraph\ntest\n"))
      )
      mdAtxParagraph shouldParseWith(
        "paragraph\nasdf\n# heading\n\u0000",
        List(Paragraph("paragraph\nasdf\n"), Heading(1,"heading"))
      )
    }
    describe("the parser shoud read a atxHeading bevor a paragraph") {
      mdAtxParagraph shouldParseWith(
        "# heading\nparagraph\n\u0000",
        List(Heading(1, "heading"),Paragraph("paragraph\n"))
      )
    }
    describe("the parser shoud get broaken by an ATX heading") {
      mdAtxParagraph shouldParseWith(
        "paragraph\n# heading\n\u0000",
        List(Paragraph("paragraph\n"),Heading(1, "heading"))
      )
    }
    describe("a paragraph should can be surrounded by empty lines") {
      mdEmptyParagraph shouldParseWith(
        "paragraph\n\n\u0000",
        List(Paragraph("paragraph\n"), EmptyLine())
      )
      mdEmptyParagraph shouldParseWith(
        "\nparagraph\n\u0000",
        List(EmptyLine(), Paragraph("paragraph\n"))
      )
    }
    describe("a paragraph can be inside of a blockQuote") {
      mdBlockParagraph shouldParseWith(
        "> paragraph\n\u0000",
        List(BlockQuote(List(Paragraph("paragraph\n"))))
      )
    }
    describe("a blockQuote can be inside of a blockQuote") {
      mdBlockParagraph shouldParseWith(
        "> > paragraph\n\u0000",
        List(BlockQuote(List(BlockQuote(List(Paragraph("paragraph\n"))))))
      )
    }/*
    describe("a blockQuote can be multiline") {
      mdBlockParagraph shouldParseWith(
        "> paragraph\n> paragraph\n\u0000",
        List(BlockQuote(List(Paragraph("paragraph\nparagraph\n"))))
      )
    }*/
    describe("a setextHeading is not a paragraph") {
      mdSetextParagraph shouldParseWith(
        "setextheading\n---\n\u0000",
        List(Heading(2,"setextheading\n"))
      )
    }
    describe("a setextHeading is not a paragraph but can followed by a paragraph") {
      mdSetextParagraph shouldParseWith(
        "setextheading\n---\nparagraph\n\u0000",
        List(Heading(2,"setextheading\n"), Paragraph("paragraph\n"))
      )
    }
  }
}
