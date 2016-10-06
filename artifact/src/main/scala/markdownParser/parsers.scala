package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  trait Block
  case class EmptyLine ()                       extends Block
  case class Heading (o: Int, a: String)        extends Block
  case class CodeBlock (a: String)              extends Block
  case class ThematicBreak()                    extends Block
  case class BlockQuote (content: List[Block]) extends Block
  case class Paragraph (content: String)        extends Block {
    def toStr(): String = { content }
  }

  val eosChar = 0.toChar

  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser =
    many(any)

  // ###########################################################################
  // ############################ emptyLine  ###################################
  // ###########################################################################
  lazy val emptyLine: Parser[Block] =
    many(space) ~> newline ^^^ (EmptyLine())

  // ###########################################################################
  // ########################### ATX Heading  ##################################
  // ###########################################################################
  lazy val atxHeading : Parser[Block] =
    openingSeq ~ atxHeadingContent <~ closingSeq  ^^ {
      case(o, a) => new Heading(o.length, a)
    }

  lazy val openingSeq: Parser[String] =
    many(space) ~> repeat('#', 1, 6) <~ some(space)

  lazy val closingSeq =
    (some(space) ~ many('#') ~ many(space) ~> newline) |
    (many(space) ~ many('#') ~ some(space) ~> newline) |
    newline

  lazy val atxHeadingContent: Parser[String] =
    not(some(space) ~ many(any)) &> (many(no('#') & no('\n')) &> inlineParser) <&
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
    biasedAlt(
    (manyN(4, ' ') ~> many(space) ~ newline),
    (many(space) ~> newline))

  // ###########################################################################
  // ####################### Fenced Code Block  ################################
  // ###########################################################################

  lazy val eos: Parser[Char] = eosChar

  lazy val fencedCodeBlock: Parser[Block] ={

    def getCodeBlockLine[T](i: Int): Parser[String] = {
      biasedAlt(
      (manyN(i, ' ') ~> many(no('\n'))),
      (withoutPrefix(some(space), many(no('\n'))))) ~ newline
    }

    def getClosingFence[T](p: Parser[T],i: Int): Parser[List[T]] = {
      (many(space) ~> min (p, i) <~ many(space) ~ newline) |
      eos ^^^ List()
    }

    // returns a Parser wich accept prefix + content but the prefix is stripped
    def withoutPrefix[T](prefix: Parser[Any], content: Parser[T]): Parser[T] = {
      (prefix ~> (not(prefix ~ many(any)) &> content)) <|
      (content)
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
    many(space) ~ (min('~', 3) | min('`', 3)) <~ many(space) ~ newline

  // ###########################################################################
  // ######################## Thematic Breaks ##################################
  // ###########################################################################
  lazy val thematicBreak: Parser [Block] = {
    def stripSpaces[T](p: Parser[T]): Parser[T] =
      ( no(' ')         >> {c => stripSpaces(p << c)}
      | charParser(' ') >> {c => stripSpaces(p)}
      | done(p)
      )

    def thematicBreakLine(c: Char): Parser[Any] = {
      many(space) ~> (stripSpaces(min(c, 3)) <& not(space ~ always))  <~ newline
    }

    (thematicBreakLine('*') |
     thematicBreakLine('-') |
     thematicBreakLine('_')) ^^ { a =>  ThematicBreak()}
  }

  // ###########################################################################
  // ######################## setext Heading ###################################
  // ###########################################################################
  lazy val setextHeading: Parser[Block] =
    many(many(space) ~> settextContent  <& not(setextUnderline)) ~ setextUnderline ^^ {
      case (a, b) => new Heading(b, a mkString)
    }

  lazy val setextUnderline: Parser[Int] =
    many(space) ~> min('-', 1) <~ many(space) ~ newline ^^ {case a => 2} |
    many(space) ~> min('=', 1) <~ many(space) ~ newline ^^ {case a => 1}

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
      done(p)                       |
      ('>' ~ space ~> readLine(p)) <|
      ('>' ~> readLine(p))

    def readLine[T](p: Parser[T]): Parser[T] =
      ( no('\n')          >> {c => readLine(p << c)}
      | charParser('\n')  >> {c => blockQuoteCombinator(p << c)}
      )

    blockQuoteCombinator(some(any) >> {c =>
      mdBlockParagraph <<< c + eosChar
    }
    ) ^^ {
      a => new BlockQuote(a)
    }
    // line &> delegate(p)
  }
  // ###########################################################################
  // ########################### MD Parser #####################################
  // ###########################################################################
  lazy val line: Parser[String] =
    many(no('\n')) ~ newline

  def readLine (open: Parser[Block]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)} |
    line >> {l =>
      altBiasedAlt(((altBreaking(open) <<< l) ~ md ^^ {case(a, b) => a ++ b}) ,
      ((open <<< l) ~ (not(open <<< l) &> md) ^^ {case(a, b) => a :: b}))     <|
      (md <<< l)
    }

  def breaking (p: Parser[Block]): Parser[List[Block]] =
    p >> {
      case a : Paragraph => (p <<< a.toStr) ~ (
        ((emptyLine <| fencedCodeBlock) <|
        (thematicBreak | blockQuote | atxHeading)) <|
        setextHeading) ^^ {
          case(a, b) => List(a,b)
        }
      case a => fail
    }

  lazy val md: NT[List[Block]] =
  altBiasedAlt(readLine(emptyLine),
  altBiasedAlt(readLine(indentedCodeBlock),
  altBiasedAlt((readLine(fencedCodeBlock) | readLine(thematicBreak) |
              readLine(blockQuote) | readLine(atxHeading)),
  altBiasedAlt(readLine(setextHeading), readLine(paragraph))))) |
  eos ^^^ (List())
  // ###########################################################################
  // ####################### simplyfied Parser #################################
  // ###########################################################################

  def altBreaking (p: Parser[Block]): Parser[List[Block]] =
    p >> {
      case a: Paragraph => (p <<< a.toStr) ~ (emptyLine <| (atxHeading | blockQuote)) ^^ {case(a,b) => List(a,b)}
      case a => fail
    }

  def altReadLine (open: Parser[Block], document: Parser[List[Block]]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)}                       |
    line >> {l =>
      altBiasedAlt(((altBreaking(open) <<< l) ~ document ^^ {case(a, b) => a ++ b}) ,
      ((open <<< l) ~ (not(open <<< l) &> document) ^^ {case(a, b) => a :: b}))   <|
      (document <<< l)
    }

  lazy val mdAtxParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(atxHeading, mdAtxParagraph),
                 altReadLine(paragraph, mdAtxParagraph)) |
                 eosChar ^^^ (List())

  lazy val mdEmptyParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(emptyLine, mdEmptyParagraph),
                 altReadLine(paragraph, mdEmptyParagraph)) |
                 eosChar ^^^ (List())

  lazy val mdBlockParagraph: NT[List[Block]] =
   altBiasedAlt(altReadLine(blockQuote, mdBlockParagraph),
                altReadLine(paragraph, mdBlockParagraph)) |
                eosChar ^^^ (List())
  lazy val mdSetextParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(setextHeading, mdSetextParagraph),
                 altReadLine(paragraph, mdSetextParagraph)) |
                 eosChar ^^^ (List())
  lazy val mdIndetedAtx: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndetedAtx),
                 altReadLine(atxHeading, mdIndetedAtx)) |
                 eosChar ^^^ (List())

  lazy val mdIndetedFenced: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndetedFenced),
                 altReadLine(fencedCodeBlock, mdIndetedFenced)) |
                 eosChar ^^^ (List())

  lazy val mdIndetedThematic: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndetedThematic),
                 altReadLine(thematicBreak, mdIndetedThematic)) |
                 eosChar ^^^ (List())

 lazy val mdIndetedSetext: NT[List[Block]] =
   altBiasedAlt(altReadLine(indentedCodeBlock, mdIndetedSetext),
                altReadLine(setextHeading, mdIndetedSetext)) |
                eosChar ^^^ (List())
}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers
  with MarkdownHelperfunctions
