package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  val eosChar = 0.toChar

  // ###########################################################################
  // ########################### ATX Heading  ##################################
  // ###########################################################################
  case class Heading(o: Int, a: List[Char]) {
    //println("Anzahl der #: " + o + "\nInhalt der Ueberschrift: " + a);
    print("<h" + o + ">");
    a.foreach(print);
    print("</h" + o + ">\n");
  }

  lazy val atxHeading : Parser[Any] =
    openingSeq ~ atxHeadingContent <~ closingSeq  ^^ {
      case(o: String , a: List[Any]) => (o.length, a)
      // case Heading(o, a) if o == a => { o + a}
      // case Heading(o, a) => { o + a}
      /* new Heading(o.length, inlineParser.parse(a).flatten)*/
    }

  lazy val openingSeq: Parser[String] =
    repeat(' ', 0, 3) ~> repeat('#', 1, 6) <~ some(space)

  lazy val closingSeq =
    (some(space) ~ many('#') ~ many(space) ~> newline) | (many(space) ~
    many('#') ~ some(space) ~> newline) | newline

  lazy val atxHeadingContent =
    not(some(space) ~ many(any)) &> (many(no('#')) &> inlineParser) <&
    not(many(any) ~ some(space))

  // ###########################################################################
  // ############################ Code Block  #############ää###################
  // ###########################################################################
  class CodeBlock(a: List[Char]) {
    // println("Inhalt des Code Blocks: " + a);
    print("<pre><code>");
    a.foreach(print);
    print("</code></pre>\n");
  }
  // Umschreiben in FCD!
  // Siehe Paper

  lazy val emptyLine : Parser[String] =
    biasedAlt((manyN(4, ' ') ~> many(space) ~ newline), many(space) ~> newline)

  lazy val codeBlockContent =
    some(no('\n'))

  lazy val codeBlockLine: Parser[String] =
    manyN(4, ' ') ~> codeBlockContent <~ newline

  lazy val indentedCodeBlock: Parser[String] =
    codeBlockLine ~ many(biasedAlt(emptyLine, codeBlockLine))

  // ###########################################################################
  // ####################### Fenced Code Block  ################################
  // ###########################################################################
  lazy val eos: Parser[Char] = eosChar
  // End of document charakter einführen
  lazy val fencedCodeBlock =
    openingFence >> {
      case (indentation: List[Char], openingFence: List[Char]) =>
        val closing = getClosingFence(openingFence.head, openingFence.length)
        many(getCodeBlockLine(indentation.length) <& not(closing)) <~ closing
    }

  lazy val openingFence: Parser[(List[Char], List[Char])] =
    repeat(' ', 0, 3) ~ (min('~', 3) | min('`', 3)) <~ many(space) ~ newline

  def getCodeBlockLine[T](i: Int): Parser[String] = {
    biasedAlt(manyN(i, ' ') ~> many(no('\n')), withoutPrefix(some(space), many(no('\n')))) ~ newline
  }

  def getClosingFence[T](p: Parser[T],i: Int): Parser[List[T]] = {
    (repeat(' ', 0, 3) ~> min (p, i) <~ many(space) ~ newline) | eos ^^^ List()
  }

  // returns a Parser wich accept prefix + content but the prefix is stripped
  def withoutPrefix[T](prefix: Parser[Any], content: Parser[T]): Parser[T] = {
    biasedAlt(prefix ~> (not(prefix ~ many(any)) &> content), content)
  }

  // ###########################################################################
  // ######################## Thematic Breaks ##################################
  // ###########################################################################
  lazy val thematicBreak =
    thematicBreakLine('*')|
    thematicBreakLine('-')|
    thematicBreakLine('_')

  def stripSpaces[T](p: Parser[T]): Parser[T] =
    ( no(' ')         >> {c => stripSpaces(p << c) }
    | charParser(' ') >> {c => stripSpaces(p)}
    | done(p)
    )

  def thematicBreakLine(c: Char): Parser[Any] = {
        repeat(' ', 0, 3) ~> stripSpaces(min(c, 3)) <& not(space ~ always)  <~ newline
  }
  // ###########################################################################
  // ######################## setext Heading ###################################
  // ###########################################################################
  lazy val setextHeading =
    paragraph ~ setextUnderline ^^ {case (a,b) => (b,a)}

  lazy val setextUnderline =
    repeat(' ', 0, 3) ~> min('-', 1) <~ many(space) ~ newline ^^ {case a => 2} |
    repeat(' ', 0, 3) ~> min('=', 1) <~ many(space) ~ newline ^^ {case a => 1}

  // ###########################################################################
  // ########################### Paragraph #####################################
  // ###########################################################################

  lazy val paragraph: Parser[List[Char]] =
    (repeat(' ', 0, 3) ~> paragraphContent ~
    many(many(space) ~> paragraphContent)) ^^ {
      case (a: List[Char], b: List[List[Char]]) => a ::: b.flatten
    }

  lazy val paragraphContent: Parser[List[Char]] =
    no(' ') ~  many(no('\n')) <~ newline ^^ {
      case (a: Char, b: List[Char]) => List(a) ++ b ++ List('\n')
    }
  // ###########################################################################
  // ########################## Block Quote ####################################
  // ###########################################################################
  lazy val blockQuote = {
    def blockQuoteCombinator[T](p: Parser[T]): Parser[T] =
      done(p) | biasedAlt ('>' ~ space ~> readLine(p),
                           '>' ~> readLine(p))

    def readLine[T](p: Parser[T]): Parser[T] =
      ( no('\n')          >> {c => readLine(p << c)}
      | charParser('\n')  >> {c => blockQuoteCombinator(p << c)}
      )

    blockQuoteCombinator(blockParser)
    // line &> delegate(p)
  }

  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser =
    many(any)
  // TODO: Blockparser schreiben
  lazy val blockParser =
    many(any) ~ newline
}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers with MarkdownHelperfunctions
