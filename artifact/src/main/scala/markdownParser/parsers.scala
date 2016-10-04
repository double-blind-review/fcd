package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  trait Block
  case class EmptyLine ()                       extends Block
  case class Heading (o: Int, a: String)        extends Block
  case class CodeBlock (a: String)              extends Block
  case class ThematicBreak()                    extends Block
  case class BlockQuote (content: String)       extends Block
  case class Paragraph (content: String)        extends Block {
    def toStr(): String = {
      content
    }
  }

  val eosChar = 0.toChar

  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser =
    many(any)
  lazy val blockParser =
    some(any)


  lazy val line: Parser[String] =
    many(no('\n')) ~ newline

  // ###########################################################################
  // ############################ emptyLine  ###################################
  // ###########################################################################
  lazy val emptyLine: Parser[Block] =
    biasedAlt(manyN(4, ' ') ~> many(space) ~ newline, many(space) ~> newline) ^^ {
      a => new EmptyLine()
    }

  // ###########################################################################
  // ########################### ATX Heading  ##################################
  // ###########################################################################
  lazy val atxHeading : Parser[Block] =
    openingSeq ~ atxHeadingContent <~ closingSeq  ^^ {
      case(o, a) => new Heading(o.length, a)
      // case Heading(o, a) if o == a => { o + a}
      // case Heading(o, a) => { o + a}
      /* new Heading(o.length, inlineParser.parse(a).flatten)*/
    }

  lazy val openingSeq: Parser[String] =
    repeat(' ', 0, 3) ~> repeat('#', 1, 6) <~ some(space)

  lazy val closingSeq =
    (some(space) ~ many('#') ~ many(space) ~> newline) | (many(space) ~
    many('#') ~ some(space) ~> newline) | newline

  lazy val atxHeadingContent: Parser[String] =
    not(some(space) ~ many(any)) &> (many(no('#')& no('\n')) &> inlineParser) <&
    not(many(any) ~ some(space))

  // ###########################################################################
  // ############################ Code Block  #############ää###################
  // ###########################################################################

  // Umschreiben in FCD!
  // Siehe Paper
  lazy val indentedCodeBlock: Parser[Block] =
    codeBlockLine ~ many(biasedAlt(emptyLineCodeBlock, codeBlockLine)) ^^ {
      case (a, b) => new CodeBlock(a::b mkString)
    }

  lazy val codeBlockContent: Parser[String] =
    some(no('\n'))

  lazy val codeBlockLine: Parser[String] =
    manyN(4, ' ') ~> codeBlockContent ~ newline

  lazy val emptyLineCodeBlock: Parser[String] =
    biasedAlt((manyN(4, ' ') ~> many(space) ~ newline), many(space) ~> newline)

  // ###########################################################################
  // ####################### Fenced Code Block  ################################
  // ###########################################################################

  lazy val eos: Parser[Char] = eosChar

  lazy val fencedCodeBlock: Parser[Block] ={

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

    openingFence >> {
      case (indentation: List[Char], openingFence: List[Char]) =>
        val closing = getClosingFence(openingFence.head, openingFence.length)
        many(getCodeBlockLine(indentation.length) <& not(closing)) <~ closing
    } ^^ {
      case (a: List[String]) => new CodeBlock(a mkString)
    }
  }

  lazy val openingFence: Parser[(List[Char], List[Char])] =
    repeat(' ', 0, 3) ~ (min('~', 3) | min('`', 3)) <~ many(space) ~ newline

  // ###########################################################################
  // ######################## Thematic Breaks ##################################
  // ###########################################################################
  lazy val thematicBreak: Parser [Block] = {
    def stripSpaces[T](p: Parser[T]): Parser[T] =
      ( no(' ')         >> {c => stripSpaces(p << c) }
      | charParser(' ') >> {c => stripSpaces(p)}
      | done(p)
      )

    def thematicBreakLine(c: Char): Parser[Any] = {
          repeat(' ', 0, 3) ~> (stripSpaces(min(c, 3)) <& not(space ~ always))  <~ newline
    }

    (thematicBreakLine('*')|
    thematicBreakLine('-')|
    thematicBreakLine('_')) ^^ { a =>  ThematicBreak()}
  }

  // ###########################################################################
  // ######################## setext Heading ###################################
  // ###########################################################################
  lazy val setextHeading: Parser[Block] =
    many(repeat(' ', 0, 3) ~> settextContent  <& not(setextUnderline)) ~ setextUnderline ^^ {
      case (a, b) => new Heading(b, a mkString)
    }
/*
  lazy val line =
    no(emptyLine) */
  lazy val setextUnderline: Parser[Int] =
    repeat(' ', 0, 3) ~> min('-', 1) <~ many(space) ~ newline ^^ {case a => 2} |
    repeat(' ', 0, 3) ~> min('=', 1) <~ many(space) ~ newline ^^ {case a => 1}

  lazy val settextContent: Parser[String] =
    no(' ') ~  many(no('\n')) ~ newline

  // ###########################################################################
  // ########################### Paragraph #####################################
  // ###########################################################################
  lazy val paragraph: Parser[Block] =
    (repeat(' ', 0, 3) ~> paragraphContent ~
    many(many(space) ~> paragraphContent)) ^^ {
      case (a: String,b: List[String]) => new Paragraph(a + (b mkString))
    }

  lazy val paragraphContent: Parser[String] =
    no(' ') ~  many(no('\n')) ~ newline
  // ###########################################################################
  // ########################## Block Quote ####################################
  // ###########################################################################
  lazy val blockQuote: Parser[Block]= {
    def blockQuoteCombinator[T](p: Parser[T]): Parser[T] =
      done(p) | biasedAlt ('>' ~ space ~> readLine(p),
                           '>' ~> readLine(p))

    def readLine[T](p: Parser[T]): Parser[T] =
      ( no('\n')          >> {c => readLine(p << c)}
      | charParser('\n')  >> {c => blockQuoteCombinator(p << c)}
      )

    blockQuoteCombinator(blockParser) ^^ {
      a => new BlockQuote(a)
    }
    // line &> delegate(p)
  }
  // ###########################################################################
  // ########################### MD Parser #####################################
  // ###########################################################################

  def readLine (open: Parser[Block]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)} |
    line >> {l =>
      (breaking(open) <<< l                           <|
      ((open <<< l) ~ md ^^ {case(a, b) => a :: b}))  <|
      md <<< l
    }

  def breaking (p: Parser[Block]): Parser[List[Block]] =
    p >> {
      case a : Paragraph => (
        ((emptyLine ~ md                                 <|
        fencedCodeBlock ~ md)                            <|
        (thematicBreak | blockQuote | atxHeading) ~ md)  <|
        setextHeading ~ md ) ^^ {case(a, b) => a :: b}
      case a => fail
    }

// continuation parsing style
lazy val md: NT[List[Block]] =
    readLine(emptyLine)         <|
    readLine(indentedCodeBlock) <|
    (
    readLine(fencedCodeBlock)   |
    readLine(thematicBreak)     |
    readLine(blockQuote)        |
    readLine(atxHeading)
    )                           <|
    readLine(setextHeading)     <|
    readLine(paragraph)         |
    eos ^^^ (List())


  def altBreaking (p: Parser[Block]): Parser[Block] =
    p >> {
      case a: Paragraph  => atxHeading  <| setextHeading
      case a => fail
    }
/*
  def altReadLine (open: Parser[Block]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)}                       |
    line >> {l =>
      ((altBreaking(open) <<< l ^^ {a => List(a)})     <|
      (open <<< l ^^ {a => List(a)}))                  <|
      (testMd <<< l)
    }*/

  def altReadLine (open: Parser[Block]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)}                       |
    line >> {l =>
      ((not(altBreaking(open)<<< l) &> open <<< l ^^ {a => List(a)}) |
      (altBreaking(open) <<< l)^^ {a => List(a)})   <|
      (testMd <<< l)
    }

  lazy val testMd: NT[List[Block]] =
    altBiasedAlt(altReadLine(atxHeading) , altReadLine(paragraph))

  lazy val testParser = many(any)

  def altBiasedAlt[T](p: Parser[T], q: Parser[T]): Parser[T] =
    testParser >> { l =>
      (p <<< l | (not((p <<< l) ~ testParser) &> q <<< l))
    }
}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers
  with MarkdownHelperfunctions
