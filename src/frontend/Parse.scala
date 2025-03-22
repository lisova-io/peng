package frontend.parse

import frontend.ast._
import frontend.lex.{Lexer, Offset, Token, Span, WithSpan}
import scala.collection.mutable.HashMap

class ParseError(val span: Span, val msg: String):
  def unapply: (Span, String) = (span, msg)

type ParseResult[T] = Either[List[ParseError], T]

private def unexpected[A, E](span: Span, actual: A, expected: E*): ParseError = {
  def formatExpected(l: Seq[E]): String =
    l match
      case Nil           => ""
      case x :: Nil      => x.toString
      case x :: y :: Nil => s"$x or $y"
      case x :: tail     => s"$x, " + formatExpected(tail)

  ParseError(span, s"Unexpected $actual. Expected ${formatExpected(expected)}")
}

class Parser(lexer: Lexer):
  def token[Tok](tok: Tok): ParseResult[Unit] =
    lexer.next
      .map(_ match
        case ws @ WithSpan(tok, s) => Right(()))
      .getOrElse(Left(List(unexpected(lexer.endSpan, "eof", "variable declaration"))))

  def token[Tok, T](matcher: Token => Boolean, fn: Tok => T): ParseResult[WithSpan[T]] = ???

  def parseFnDecl: ParseResult[FnDecl] = ???

  def parseType: ParseResult[WithSpan[Type]] = ???

  def parseExpr: ParseResult[Expr] = ???

  def parseVarDecl(const: Boolean): ParseResult[FnDecl] = {
    val const = token(_ == Token.Val, _ => true).orElse(token(_ == Token.Var, _ => false))
    val name = token[Token.Identifier, Name](
      (_ == Token.Identifier),
      (_.value)
    )

    val typeResult: ParseResult[Option[WithSpan[Type]]] =
      lexer.peek
        .map(ws =>
          ws match
            case WithSpan(Token.Colon, _) => token(Token.Colon).flatMap(_ => parseType).map(Some(_))
            case WithSpan(Token.Assign, _) => Right(None)
            case WithSpan(tok, span) => Left(List(unexpected(span, tok, "variable declaration")))
        )
        .getOrElse(Left(List(unexpected(lexer.endSpan, "eof", "variable declaration"))))

    const.flatMap(const =>
      name.flatMap(name =>
        typeResult.flatMap(t =>
          // todo: value
          token(Token.Assign)
            .flatMap(_ => parseExpr)
            .map(VarDecl(const.value, name, t)) // got concrete type
        )
      )
    )
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
