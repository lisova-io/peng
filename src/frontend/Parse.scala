package frontend.parse

import cats.parse.{Parser, Parser0}
import cats.parse.Rfc5234._
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation._

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
def tp: Parser[Type]    = identifier

def expr: Parser[Expr] = ???

def varDecl: Parser[VarDecl] =
  def mkVarDecl(args: ((String, Option[Type]), Expr)): VarDecl = {
    val ((name, t), value) = args
    VarDecl(name, t, value)
  }
  (identifier ~ (token(':') *> tp).? ~ (token('=') *> expr)).map(mkVarDecl)

def stmt: Parser[Stmt] = varDecl.map(DeclStmt(_)).backtrack | ???

// def parse(): Parser[AST] =
