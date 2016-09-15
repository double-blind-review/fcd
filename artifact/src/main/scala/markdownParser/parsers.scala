package fcd
package markdown

trait MarkdownParsers { self: RichParsers with MarkdownHelperfunctions =>

  // ###########################################################################
  // ########################### ATX Heading  ##################################
  // ###########################################################################
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

  // ###########################################################################
  // ############################ Code Block  #############ää###################
  // ###########################################################################
  class CodeBlock(a: List[Char]) {
    // println("Inhalt des Code Blocks: " + a);
    print("<pre><code>");
    a.foreach(print);
    print("</code></pre>\n");
  }

  lazy val emptyLine : Parser[List[Char]] =
    biasedAlt((manyN(4, ' ') ~> many(space) ~ newline) , (many(space) ~> newline)) ^^ {
      case (a: Char) => List(a);
      case (p:List[Char], ps:Char) => p ++ List(ps)
    }

  lazy val codeBlockContent =
    some(no('\n'))

  lazy val codeBlockLine: Parser[List[Char]] =
    manyN(4, ' ') ~> codeBlockContent <~ newline ^^ {
      case(a: List[Char]) => a ++ List('\n')
    }

  lazy val indentedCodeBlock =
    (codeBlockLine ~ many(biasedAlt(emptyLine, codeBlockLine))) ^^ {
      case (first: List[Char], rest: List[List[Char]]) => first ++ rest.flatten
    }
  // ###########################################################################
  // ####################### Fenced Code Block  ################################
  // ###########################################################################
  lazy val fencedCodeBlock =
    openingFence >> {
      case (indentation: List[Char], openingFence: List[Char]) =>
      (many(getCodeBlockLine(indentation.length) <& not(getClosingFence(openingFence.head, openingFence.length))) <~
      biasedAlt(getClosingFence(openingFence.head, openingFence.length) , succeed(Nil)))
    } ^^ {
      case(a: List[List[Char]]) => a.flatten
    }

  lazy val openingFence: Parser[(List[Char], List[Char])] =
    repeat(' ', 0, 3) ~ (min('~', 3) | min('`', 3)) <~ many(space) ~ newline

  lazy val openingfence =
    repeat(' ', 0, 3) ~> (min('~', 3) | min('`', 3)) <~ many(space) ~ newline

  def getCodeBlockLine[T](i: Int): Parser[List[Char]] = {
    biasedAlt(manyN(i, ' ') ~> many(no('\n')), withoutPrefix(some(space), many(no('\n')))) <~ newline ^^ {
      case(line: List[Char]) => line ++ List('\n')
    }
  }

  def getClosingFence[T](p: Parser[T],i: Int): Parser[List[T]] = {
    repeat(' ', 0, 3) ~> min (p, i) <~ many(space) ~ newline
  }

  // returns a Parser wich accept prefix + content but the prefix is stripped
  def withoutPrefix[T](prefix: Parser[T], content: Parser[T]): Parser[T] = {
    biasedAlt(prefix ~> (not(prefix ~ many(any)) &> content), content)
  }



  // Hier könnte ihr Markdown Inline Parser stehen
  lazy val inlineParser =
    many(any)
  // TODO: Blockparser schreiben
  lazy val blockParser =
    many(any)
}

object MarkdownParsers extends MarkdownParsers with RichParsers with DerivativeParsers with MarkdownHelperfunctions
