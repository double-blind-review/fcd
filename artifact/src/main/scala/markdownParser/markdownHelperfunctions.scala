package fcd
package markdown

trait MarkdownHelperfunctions{ self: RichParsers =>

  //def biasedAlt[T](p: Parser[T], q: Parser[T]): Parser[T] =
  //  p | not(p ~ many(any)) &> q

  // TODO: ist es möglich ein Parser zu haben der genau 0 zeichen liest?

  def min[T](p: Parser[T], i: Int): Parser[List[T]] = {
    manyN(i, p) ~ many(p) ^^ {case (a,b) => a ++ b}
  }

  def max[T](p: Parser[T], i: Int): Parser[List[T]] = {
    if (i == 0) succeed(Nil)
    else (manyN(i, p) | max(p, i - 1))
  }

  def repeat[T](p: Parser[T], i: Int , j: Int): Parser[List[T]] =
    min(p, i) &> max(p, j)
}
