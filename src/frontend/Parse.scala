package frontend.parse

import frontend.ast._
import frontend.lex.{Lexer, Offset, Token, Span, WithSpan}
import scala.collection.mutable.HashMap

class ParseError(val span: Span, val msg: String):
  def unapply: (Span, String) = (span, msg)

type ParseResult[T] = Either[List[ParseError], T]

private def unexpected[A](span: Span, actual: A, expected: Any*): ParseError = {
  def formatExpected(l: Seq[Any]): String =
    l match
      case Nil           => ""
      case x :: Nil      => x.toString
      case x :: y :: Nil => s"$x or $y"
      case x :: tail     => s"$x, " + formatExpected(tail)

  ParseError(span, s"Unexpected $actual. Expected ${formatExpected(expected)}")
}

class Parser(lexer: Lexer):
  private def unexpectedEof(expected: Any*): ParseError = unexpected(lexer.endSpan, "eof", expected)

  private def token[T](tok: Token, value: T = ()): ParseResult[WithSpan[T]] =
    lexer.next
      .map(ws =>
        if ws.value.is(tok)
        then Right(ws.map(_ => value))
        else Left(List(unexpected(ws.span, ws.value, tok)))
      )
      .getOrElse(Left(List(unexpected(lexer.endSpan, "eof", tok))))

  private def tryToken[T](tok: Token, value: T = ()): ParseResult[WithSpan[T]] =
    lexer.peek
      .map(ws =>
        if ws.value.is(tok)
        then { lexer.next; Right(ws.map(_ => value)) }
        else Left(List(unexpected(ws.span, ws.value, tok)))
      )
      .getOrElse(Left(List(unexpected(lexer.endSpan, "eof", tok))))

  private def parseNumber: ParseResult[WithSpan[Int]] =
    lexer.next
      .map(ws =>
        ws match
          case WithSpan(Token.Number(v), s) => Right(ws.map(_ => v))
          case WithSpan(t, s)               => Left(List(unexpected(s, t, "number")))
      )
      .getOrElse(Left(List(unexpectedEof("number"))))

  private def parseIdentifier: ParseResult[WithSpan[String]] =
    lexer.next
      .map(ws =>
        ws match
          case WithSpan(Token.Identifier(v), s) => Right(ws.map(_ => v))
          case WithSpan(t, s)                   => Left(List(unexpected(s, t, "identifier")))
      )
      .getOrElse(Left(List(unexpectedEof("identifier"))))

  private def parseType: ParseResult[WithSpan[Type]] = ???

  private def parseExpr: ParseResult[Expr] = ???

  private def parseFnDecl: ParseResult[FnDecl] = ???

  private def parseVarDecl(const: Boolean): ParseResult[VarDecl] = {
    val const = tryToken(Token.Val, true).orElse(token(Token.Var, false))
    val name  = parseIdentifier

    val typeResult: ParseResult[Option[WithSpan[Type]]] =
      lexer.peek
        .map(_ match
          case WithSpan(Token.Colon, _)  => token(Token.Colon).flatMap(_ => parseType).map(Some(_))
          case WithSpan(Token.Assign, _) => Right(None)
          case WithSpan(tok, span) => Left(List(unexpected(span, tok, "variable declaration"))))
        .getOrElse(Left(List(unexpectedEof("variable declaration"))))

    val value = token(Token.Assign).flatMap(_ => parseExpr)

    for {
      const <- const
      name  <- name
      t     <- typeResult
      value <- value
    } yield VarDecl(const.value, name, t, value)
  }

  def parse: ParseResult[AST] = {
    var res: AST                 = HashMap[Name, Decl]()
    var errors: List[ParseError] = List()

    while lexer.peek.isDefined do {
      val declRes = lexer.next.get match
        case WithSpan(Token.Fn, _)  => parseFnDecl
        case WithSpan(Token.Val, _) => parseVarDecl(const = true)
        case WithSpan(Token.Var, _) => parseVarDecl(const = false)
        case WithSpan(t, s)         => Left(List(unexpected(s, t, "declaration")))

      declRes match
        case Left(errs) => errors ++= errs
        // TODO: handle name duplication
        // case Right(decl) if res.contains(decl.getName) => errors :+= ParseError(i)
        case Right(decl) => res += (decl.name.value, decl)
    }

    if errors.isEmpty then Right(res) else Left(errors)
  }
