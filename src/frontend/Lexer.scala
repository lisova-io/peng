package frontend.lex

import frontend.lex.Token

extension (c: Char)
  def isIdentifierStart = c.isLetter || c == '_'
  def isIdentifierChar  = c.isLetterOrDigit || c == '_'

type Offset = Int

class Span(val b: Offset, val e: Offset):
  def len: Int = e - b + 1

implicit object SpanOrdering extends Ordering[Span]:
  override def compare(x: Span, y: Span): Int = if x.b == y.b then x.e - y.e else x.b - y.b

object Span:
  def unapply(s: Span): (Offset, Offset) = (s.b, s.e)

class WithSpan[T](val value: T, val span: Span):
  def map[U](fn: T => U): WithSpan[U] = WithSpan(fn(value), span)

object WithSpan:
  def unapply[T](ws: WithSpan[T]): (T, Span) = (ws.value, ws.span)

class Lexer(val input: String):
  private var offset: Offset = 0

  private def peekChar: Option[Char] = if offset < input.length then Some(input(offset)) else None
  private def nextChar: Option[Char] = peekChar.map(c => { offset += 1; c })

  private def skipWhile(pred: Char => Boolean): Unit =
    while peekChar.filter(pred).isDefined do nextChar

  private def getWhile(pred: Char => Boolean): String = {
    var res = String()
    while peekChar.filter(pred).isDefined do res += nextChar.get
    res
  }

  def endSpan: Span = Span(input.length - 1, input.length - 1)

  def peek: Option[WithSpan[Token]] = {
    val prevOffset = offset
    val res        = next
    offset = prevOffset
    res
  }

  def next: Option[WithSpan[Token]] = {
    skipWhile(_.isWhitespace)
    val b = offset
    nextChar
      .map(c => {
        c match
          case '(' => Token.LParen    -> Span(b, b)
          case ')' => Token.RParen    -> Span(b, b)
          case '{' => Token.LBrace    -> Span(b, b)
          case '}' => Token.RBrace    -> Span(b, b)
          case ',' => Token.Comma     -> Span(b, b)
          case '.' => Token.Dot       -> Span(b, b)
          case ':' => Token.Colon     -> Span(b, b)
          case ';' => Token.Semicolon -> Span(b, b)
          case '+' => Token.Plus      -> Span(b, b)
          case '-' => Token.Minus     -> Span(b, b)
          case '*' => Token.Asterisk  -> Span(b, b)
          case '=' => Token.Assign    -> Span(b, b)
          case c if c.isIdentifierStart => {
            (c + getWhile(_.isIdentifierChar)) match
              case "fn"     => Token.Fn            -> Span(b, b + 1)
              case "val"    => Token.Val           -> Span(b, b + 2)
              case "var"    => Token.Var           -> Span(b, b + 2)
              case "return" => Token.Return        -> Span(b, b + 5)
              case s        => Token.Identifier(s) -> Span(b, b + s.length - 1)
          }
          case c if c.isDigit => {
            val raw = c + getWhile(_.isDigit)
            Token.Number(raw.toInt) -> Span(b, b + raw.length - 1)
          }
      })
      .map((tok, span) => WithSpan(tok, span))
  }
