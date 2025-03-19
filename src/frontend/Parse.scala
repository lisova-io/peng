package frontend.parse

import cats.parse.{Parser, Parser0}
import cats.parse.Rfc5234._
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation._
import cats.parse.Parser.Error

import scala.collection.mutable.HashMap

import frontend.ast._

private def intFromDigits(digits: Seq[Int]): Int = {
  var res = 0
  for (digit <- digits) {
    res *= 10
    res += digit
  }
  return res
}

def whitespace0: Parser0[Unit]         = Parser.charIn(" \t\r\n").rep0.void
def symbol[T](p: Parser[T]): Parser[T] = p.surroundedBy(whitespace0)
def identifier: Parser[String] =
  symbol(alpha ~ (alpha.backtrack | digit).rep.string).map((c, s) => c + s)
def number: Parser[Int] = symbol(digit.rep).map(x => intFromDigits(x.map(_.toInt - '0').toList))
def token(ch: Char)     = symbol(Parser.char(ch))
def token(s: String)    = symbol(Parser.string(s))
def parens[T](p: Parser[T]): Parser[T]   = p.between(token('('), token(')'))
def parens[T](p: Parser0[T]): Parser0[T] = p.between(token('('), token(')'))
def braces[T](p: Parser[T]): Parser[T]   = p.between(token('{'), token('}'))
def braces[T](p: Parser0[T]): Parser0[T] = p.between(token('{'), token('}'))
def tp: Parser[Type]                     = identifier

def expr: Parser[Expr]    = ???
def block: Parser0[Block] = braces(stmt.repSep0(token(';')))

def varDecl: Parser[VarDecl] = {
  val isConst: Parser[Boolean] = token("val").as(true).backtrack | token("var").as(false)

  (isConst ~ identifier ~ (token(':') *> tp).backtrack.? ~ (token('=') *> expr))
    .map(in => {
      val (((const, name), t), value) = in
      VarDecl(const, name, t, value)
    })
}

def fnDecl: Parser[FnDecl] =
  def fnArg: Parser[(Name, Type)] = identifier ~ (token(':') *> tp)

  (token("fn") *> identifier ~ parens(fnArg.repSep0(token(','))) ~ (token(
    ':'
  ) *> tp) ~ block)
    .map(in => {
      val (((name, args), rettype), body) = in
      FnDecl(name, args, rettype, body)
    })

def stmt: Parser[Stmt] = varDecl.map(DeclStmt(_)).backtrack | ???
def decl: Parser[Decl] = varDecl.backtrack | fnDecl

def parse(input: String): Either[Error, AST] =
  decl.rep0.parse(input).map(r => HashMap().addAll(r._2.map(d => d.getName -> d)))
