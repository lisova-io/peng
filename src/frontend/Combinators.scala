package frontend.parse.combinators

import frontend.lex.Tokens
import frontend.lex.Token
import frontend.lex.Span
import frontend.lex.WithSpan
import frontend.diagnostics.Diagnostic
import frontend.diagnostics.Message

// TODO: FIX EXPECTED

private def makeExpectedDiag[A](span: Span, expected: Any*): Diagnostic = {
  def formatExpected(l: Seq[Any]): String =
    l match
      case Nil           => ""
      case x +: Nil      => x.toString
      case x +: y +: Nil => s"$x or $y"
      case x +: tail     => s"$x, " + formatExpected(tail)

  Diagnostic.error(span, s"expected ${formatExpected(expected)}")
}

class Parser[T](
    val run: Tokens => (Either[Diagnostic, T], Tokens),
    val expected: Seq[Any] = Nil,
):
  def map[U](f: T => U): Parser[U] =
    Parser(in => {
      val (v, outStream) = run(in)
      v.map(f) -> outStream
    })
  def flatMap[U](f: T => Parser[U]): Parser[U] =
    Parser(in => {
      val (v, out) = run(in)
      v match
        case Left(err) => Left(err) -> out
        case Right(v)  => f(v).run(out)
    })
  def andThen[U](that: => Parser[U]): Parser[U] = flatMap(_ => that)
  def foreach(f: T => Unit): Parser[Unit]       = map(f)
  def orElse[U <: V, V >: T](that: => Parser[U]): Parser[V] =
    Parser(
      in => {
        val (v, out1) = run(in)
        v match
          case Left(err) => that.run(out1)
          case Right(_)  => v -> out1
      },
      expected :++ that.expected,
    )
  def `<|>`[U <: V, V >: T](that: => Parser[U]): Parser[V] = orElse(that)
  def replace[U](x: U): Parser[U]                          = map(_ => x)
  def backtrack: Parser[T] =
    Parser(in =>
      run(in) match
        case (Left(err), _) => Left(err) -> in
        case r              => r
    )
  def lookahead: Parser[T] =
    Parser(in =>
      run(in) match
        case (Right(v), _) => Right(v) -> in
        case l             => l
    )

  def some: Parser[List[T]] =
    def some_v: Parser[List[T]] =
      for {
        v  <- this
        mv <- many_v
      } yield v +: mv
    def many_v: Parser[List[T]] = some_v <|> Parser.pure(Nil)
    some_v

  def many: Parser[List[T]] =
    def some_v: Parser[List[T]] =
      for {
        v  <- this
        mv <- many_v
      } yield v +: mv
    def many_v: Parser[List[T]] = some_v <|> Parser.pure(Nil)
    many_v
  def expect(e: Seq[Any]): Parser[T] =
    Parser(
      in =>
        val (res, toks) = run(in)
        (res match
          case Left(diag) => Left(makeExpectedDiag(diag.firstMsg.span, e*))
          case r          => r
        ) -> toks
      ,
      e,
    )
  def void: Parser[Unit]                                   = replace(())
  def ignoreFailure[U <: V, V >: T](default: U): Parser[V] = this <|> Parser.pure(default)
  def `<*`(that: => Parser[?]): Parser[T]                  = flatMap(x => that.replace(x))
  def `*>`[U](that: => Parser[U]): Parser[U]               = andThen(that)
  def sepBy[Sep](sep: => Parser[Sep]): Parser[List[T]]     = sepBy1(sep) <|> Parser.pure(Nil)
  def sepBy1[Sep](sep: => Parser[Sep]): Parser[List[T]] =
    for {
      head <- this
      tail <- (sep *> this).many
    } yield head +: tail
  def sepEndBy[Sep](sep: => Parser[Sep]): Parser[List[T]] = sepEndBy1(sep) <|> Parser.pure(Nil)
  def sepEndBy1[Sep](sep: => Parser[Sep]): Parser[List[T]] =
    for {
      head <- this
      tail <- (sep *> sepEndBy(sep)) <|> Parser.pure(Nil)
    } yield head +: tail

object Parser:
  def pure[T](x: T, expected: Seq[Any] = Nil): Parser[T] = Parser(Right(x) -> _, expected)
  def fail[T](err: Diagnostic): Parser[T]                = Parser(Left(err) -> _)
  def skipWhile(pred: Token => Boolean): Parser[Unit] =
    Parser.token(t => Some(()).filter(_ => pred(t.value))).many.replace(())
  def skipUntil(pred: Token => Boolean): Parser[Unit] = Parser.skipWhile(!pred(_))
  def token[T](
      filterMap: WithSpan[Token] => Option[T],
      expected: Seq[Any] = Nil,
  ): Parser[T] =
    Parser(
      toks =>
        toks.next match
          case (Some(ws), rest) =>
            filterMap(ws) match
              case None      => Left(makeExpectedDiag(ws.span, expected*)) -> rest
              case Some(res) => Right(res)                                 -> rest
          case (None, rest) => Left(makeExpectedDiag(toks.endSpan, expected*)) -> rest
      ,
      expected,
    )
