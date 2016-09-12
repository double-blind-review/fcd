package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  // ATX heading
  class Heading(o: Int, a: List[Char]) {
    //println("Anzahl der #: " + o + "\nInhalt der Ueberschrift: " + a);
    print("<h" + o + ">");
    a.foreach(print);
    print("</h" + o + ">\n");
  }

  lazy val atxHeading : Parser[Any] =
    openingSeq ~ atxHeadingContent <~ closingSeq  ^^ {
      case(o: String , a: List[Any]) => (o.length, a)
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



  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser =
    many(any)

}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers with MarkdownHelperfunctions
