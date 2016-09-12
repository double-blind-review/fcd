package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  // ATX heading

  val openingSeq: Parser[String] = repeat(' ', 0, 3) ~> repeat('#', 1, 6) <~ some(space)
/*
  val closingSeq =
    (some(space) ~ many('#') ~ many(space) ~> newline) | (many(space) ~
    many('#') ~ some(space) ~> newline) | newline

  val atxHeadingContent =
    not(some(space) ~ many(any)) &> (many(no('#')) &> inlineParser) <& not(many(any) ~ some(space))

  // Hier könnte ihr Markdown Inline Parser stehen
  val inlineParser =
    many(any)

  val atxHeading : Parser[Any] =
    openingSeq ~ atxHeadingContent <~ closingSeq  ^^ {
      case(o: String , a: List[Any]) => (a, o)
  }

  // o = die überschriften größe
  // a = der überschriften Text
*/
}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers with MarkdownHelperfunctions
