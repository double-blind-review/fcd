package fcd
package markdown

import scala.language.postfixOps

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  trait Block
  case class EmptyLine ()                       extends Block
  case class Heading (o: Int, a: String)        extends Block
  case class CodeBlock (a: String)              extends Block
  case class ThematicBreak()                    extends Block
  case class BlockQuote (content: List[Block])  extends Block
  case class Paragraph (content: String)        extends Block {
    def toStr(): String = { content }
  }

  lazy val indet = repeat(space, 0, 3)
  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser = many(any)
  // parser that pareses the end of stream char
  val eosChar = 0.toChar
  lazy val eos: Parser[Char] = eosChar

  // ###########################################################################
  // ############################ emptyLine  ###################################
  // ###########################################################################

  lazy val emptyLine: Parser[Block] = many(space) ~> newline ^^^ (EmptyLine())

  // ###########################################################################
  // ########################### ATX Heading  ##################################
  // ###########################################################################

  lazy val atxHeading : Parser[Block] =
    openingSeq ~ atxHeadingContent <~ closingSeq ^^ {
      case (a, b) => new Heading(a.length, b)
    }

  lazy val openingSeq: Parser[String] =
    many(space) ~> repeat('#', 1, 6) <~ some(space)

  lazy val closingSeq =
    ( (some(space) ~ many('#') ~ many(space) ~> newline)
    | (many(space) ~ many('#') ~ some(space) ~> newline)
    | newline
    )

  lazy val atxHeadingContent: Parser[String] =
    ( not(some(space) ~ many(any))
    &> (many(no('#') & no('\n')) &> inlineParser)
    <& not(many(any) ~ some(space))
    )

  // ###########################################################################
  // ############################ Code Block  #############ää###################
  // ###########################################################################

  lazy val indentedCodeBlock: Parser[Block] =
    codeBlockLine ~ many(biasedAlt(emptyLineCodeBlock, codeBlockLine)) ^^ {
      case (a, b) => new CodeBlock(a :: b mkString)
    }

  lazy val codeBlockContent: Parser[String] =
    some(no('\n'))

  lazy val codeBlockLine: Parser[String] =
    manyN(4, space) ~> codeBlockContent ~ newline

  lazy val emptyLineCodeBlock: Parser[String] =
    biasedAlt((manyN(4, space) ~> many(space) ~ newline)
             ,(many(space) ~> newline)
             )

  // ###########################################################################
  // ####################### Fenced Code Block  ################################
  // ###########################################################################

  lazy val fencedCodeBlock: Parser[Block] ={

    def getCodeBlockLine[T](i: Int): Parser[String] =
      ( (manyN(i, space) ~> many(no('\n')))
      <|(withoutPrefix(some(space), many(no('\n'))))
      ) ~ newline


    def getClosingFence[T](p: Parser[T],i: Int): Parser[List[T]] =
      ( (indet ~> min (p, i) <~ many(space) ~ newline)
      | eos ^^^ List()
      )

    // returns a Parser wich accept prefix + content but the prefix is stripped
    def withoutPrefix[T](prefix: Parser[Any], content: Parser[T]): Parser[T] =
      (prefix ~> (not(prefix ~ many(any)) &> content)) <| (content)

    openingFence >> {
      case (indentation, openingFence) =>
        val closing = getClosingFence(openingFence.head, openingFence.length)
        many(getCodeBlockLine(indentation.length) <& not(closing)) <~ closing
    } ^^ {a => new CodeBlock(a mkString)}
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

    def thematicBreakLine(c: Char): Parser[Any] =
      many(space) ~> (stripSpaces(min(c, 3)) <& not(space ~ always)) <~ newline

    ( thematicBreakLine('*')
    | thematicBreakLine('-')
    | thematicBreakLine('_')
    ) ^^ { a =>  ThematicBreak()}
  }

  // ###########################################################################
  // ######################## setext Heading ###################################
  // ###########################################################################

  lazy val setextHeading: Parser[Block] =
    many(indet ~> settextContent  <&
    not(setextUnderline)) ~ setextUnderline ^^ {
      case (a, b) => new Heading(b, a mkString)
    }

  lazy val setextUnderline: Parser[Int] =
    ( indet ~> min('-', 1) <~ many(space) ~ newline ^^^ (2)
    | indet ~> min('=', 1) <~ many(space) ~ newline ^^^ (1)
    )

  lazy val settextContent: Parser[String] =
    no(' ') ~ many(no('\n')) ~ newline

  // ###########################################################################
  // ########################### Paragraph #####################################
  // ###########################################################################

  lazy val paragraph: Parser[Block] =
    (indet ~> paragraphContent ~
    many(many(space) ~> paragraphContent)) ^^ {
      case (a, b) => new Paragraph(a + (b mkString))
    }

  lazy val paragraphContent: Parser[String] =
    no(' ') ~ many(no('\n')) ~ newline

  // ###########################################################################
  // ########################## Block Quote ####################################
  // ###########################################################################

  lazy val blockQuote: Parser[Block]= {
    def blockQuoteCombinator[T](p: Parser[T]): Parser[T] =
      ( done(p)
      | ('>' ~ space ~> readLine(p))
      <|('>' ~> readLine(p))
      )

    def readLine[T](p: Parser[T]): Parser[T] =
      ( no('\n')          >> {c => readLine(p << c)}
      | charParser('\n')  >> {c => blockQuoteCombinator(p << c)}
      )

    blockQuoteCombinator(some(any) >> {c =>
      mdBlockParagraph <<< c + eosChar
    }) ^^ {a => new BlockQuote(a)}
  }
  // ###########################################################################
  // ########################### MD Parser #####################################
  // ###########################################################################

  lazy val line: Parser[String] =
    many(no('\n')) ~ newline

  def readLine (open: Parser[Block]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)} |
    line >> {l =>
      altBiasedAlt(((altBreaking(open) <<< l) ~ md ^^ {case (a, b) => a ++ b}),
      ((open <<< l) ~ (not(open <<< l) &> md) ^^ {case (a, b) => a :: b}))    <|
      (md <<< l)
    }

  def breaking (p: Parser[Block]): Parser[List[Block]] =
    p >> {
      case a : Paragraph => (p <<< a.toStr) ~
        ( ((emptyLine <| fencedCodeBlock)
        <| (thematicBreak | blockQuote | atxHeading))
        <| setextHeading
        ) ^^ {
          case(a, b) => List(a, b)
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
      case a: Paragraph => (p <<< a.toStr) ~
        (emptyLine <| (atxHeading | blockQuote)) ^^ {
          case (a, b) => List(a, b)
        }
      case a => fail
    }

  def altReadLine (open: Parser[Block], doc: Parser[List[Block]]): Parser[List[Block]] =
    done(open) ^^ {a => List(a)} |
    line >> {l =>
      altBiasedAlt(((altBreaking(open) <<< l) ~ doc ^^ {
        case (a, b) => a ++ b
      }) ,
      ((open <<< l) ~ (not(open <<< l) &> doc) ^^ {
        case (a, b) => a :: b
      })) <|
      (doc <<< l)
    }

  lazy val mdAtxParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(atxHeading, mdAtxParagraph),
                 altReadLine(paragraph, mdAtxParagraph)) |
                 eos ^^^ (List())

  lazy val mdEmptyParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(emptyLine, mdEmptyParagraph),
                 altReadLine(paragraph, mdEmptyParagraph)) |
                 eos ^^^ (List())

  lazy val mdBlockParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(blockQuote, mdBlockParagraph),
                 altReadLine(paragraph, mdBlockParagraph)) |
                 eos ^^^ (List())

  lazy val mdSetextParagraph: NT[List[Block]] =
    altBiasedAlt(altReadLine(setextHeading, mdSetextParagraph),
                 altReadLine(paragraph, mdSetextParagraph)) |
                 eos ^^^ (List())

  lazy val mdIndentedAtx: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndentedAtx),
                 altReadLine(atxHeading, mdIndentedAtx)) |
                 eos ^^^ (List())

  lazy val mdIndentedFenced: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndentedFenced),
                 altReadLine(fencedCodeBlock, mdIndentedFenced)) |
                 eos ^^^ (List())

  lazy val mdIndentedThematic: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndentedThematic),
                 altReadLine(thematicBreak, mdIndentedThematic)) |
                 eos ^^^ (List())

  lazy val mdIndentedSetext: NT[List[Block]] =
    altBiasedAlt(altReadLine(indentedCodeBlock, mdIndentedSetext),
                 altReadLine(setextHeading, mdIndentedSetext)) |
                 eos ^^^ (List())
}

object MarkdownParsers extends MarkdownParsers with RichParsers
  with DerivativeParsers with MarkdownHelperfunctions
