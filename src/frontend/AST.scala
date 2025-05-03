package frontend
package ast

import scala.collection.mutable.HashMap

import lex.{Span, WithSpan}
import types.Type
import translationUnit.Name
import treePrinter.makeOffset

sealed trait AstNode
sealed trait Decl(
    val getName: WithSpan[Name],
    val getType: WithSpan[Type],
) extends AstNode
sealed trait Stmt extends AstNode
sealed trait Expr extends AstNode:
  def getSpan: Span

type AST   = HashMap[Name, Decl]
type Block = List[Stmt]

sealed case class FnDecl(
    val name: WithSpan[Name],
    val params: List[(WithSpan[Name], WithSpan[Type])],
    val rettype: WithSpan[Type],
    val body: BlockStmt,
) extends Decl(name, rettype)

sealed case class VarDecl(
    val const: Boolean,
    val name: WithSpan[Name],
    val tp: WithSpan[Type],
    val value: Expr,
) extends Decl(name, tp)

sealed case class ExprStmt(val expr: Expr)    extends Stmt
sealed case class BlockStmt(val block: Block) extends Stmt
sealed case class IfStmt(
    val cond: Expr,
    val onTrue: BlockStmt,
    val onFalse: Option[BlockStmt | IfStmt],
) extends Stmt
sealed case class WhileStmt(
    val cond: Expr,
    val body: BlockStmt,
) extends Stmt
sealed case class DeclStmt(val decl: VarDecl) extends Stmt
sealed case class RetStmt(val expr: Expr)     extends Stmt
sealed case class UnitRetStmt(val span: Span) extends Stmt

type Precedence = Int

enum UnaryOp {
  case Minus

  override def toString: String =
    this match
      case Minus => "-"
}

enum BinOp extends Ordered[BinOp] {
  case Plus
  case Minus
  case Mul
  case Assign
  case Eq
  case Ne
  case Lt
  case Le
  case Gt
  case Ge

  override def toString: String =
    this match
      case Plus   => "+"
      case Minus  => "-"
      case Mul    => "*"
      case Assign => "="
      case Eq     => "=="
      case Ne     => "!="
      case Lt     => "<"
      case Le     => "<="
      case Gt     => ">"
      case Ge     => ">="

  override def compare(that: BinOp): Int = this.precedence compare that.precedence

  def precedence: Precedence = this match
    case Assign            => 1
    case Lt | Le | Gt | Ge => 2
    case Eq | Ne           => 3
    case Plus | Minus      => 4
    case Mul               => 5

  def isCmp: Boolean = this match
    case Eq | Ne | Lt | Le | Gt | Ge => true
    case _                           => false
}

sealed case class VarRefExpr(val name: WithSpan[Name], val tp: Type) extends Expr:
  override def toString: String = s"${name.value} $tp"
  override def getSpan: Span    = name.span

sealed case class CallExpr(val name: WithSpan[Name], val args: List[Expr]) extends Expr:
  override def toString: String = s"(${name.value} (${args.mkString(", ")}))"
  override def getSpan: Span    = name.span

sealed case class BinExpr(val op: WithSpan[BinOp], val lhs: Expr, val rhs: Expr, val tp: Type)
    extends Expr:
  override def toString: String = s"($lhs ${op.value} $rhs)"
  override def getSpan: Span    = op.span

sealed case class UnaryExpr(val op: WithSpan[UnaryOp], val expr: Expr) extends Expr:
  override def toString: String = s"(${op.value} $expr)"
  override def getSpan: Span    = op.span

sealed case class NumLitExpr(val tp: Type, val value: WithSpan[BigInt]) extends Expr:
  override def toString: String = value.value.toString
  override def getSpan: Span    = value.span

sealed case class BoolLitExpr(val value: WithSpan[Boolean]) extends Expr:
  override def toString: String = value.value.toString
  override def getSpan: Span    = value.span

private def printNode(expr: Expr, depth: Int): Unit =
  makeOffset(depth)
  expr match
    case BinExpr(WithSpan(op, _), lhs, rhs, tp) =>
      println(s"BinExpr $op $tp")
      printNode(lhs, depth + 1)
      printNode(rhs, depth + 1)
    case VarRefExpr(WithSpan(name, _), tp) =>
      println(s"VarRefExpr $name $tp")
    case CallExpr(WithSpan(name, _), args) =>
      println(s"CallExpr $name")
      args.foreach(e => printNode(e, depth + 1))
    case UnaryExpr(WithSpan(op, _), e) =>
      println(s"UnaryExpr $op")
      printNode(e, depth + 1)
    case NumLitExpr(t, WithSpan(v, _)) =>
      println(s"NumLitExpr $t $v")
    case BoolLitExpr(WithSpan(v, _)) =>
      println(s"BoolLitExpr $v")

private def printNode(stmt: Stmt, depth: Int): Unit =
  makeOffset(depth)
  stmt match
    case BlockStmt(block) =>
      println("BlockStmt")
      block.foreach(s => printNode(s, depth + 1))
    case ExprStmt(e) =>
      println("ExprStmt")
      printNode(e, depth + 1)
    case DeclStmt(d) =>
      println("DeclStmt")
      printNode(d, depth + 1)
    case RetStmt(e) =>
      println("RetStmt")
      printNode(e, depth + 1)
    case IfStmt(cond, onTrue, onFalse) =>
      println("IfStmt")
      printNode(cond, depth + 1)
      printNode(onTrue, depth + 1)
      if onFalse.isDefined then printNode(onFalse.get, depth + 1)
    case WhileStmt(cond, body) =>
      println("WhileStmt")
      printNode(cond, depth + 1)
      printNode(body, depth + 1)
    case UnitRetStmt(_) => println("VoidRetStmt")

def printNode(decl: Decl, depth: Int = 0): Unit =
  makeOffset(depth)
  decl match
    case FnDecl(WithSpan(name, _), params, rettype, body) =>
      println(
        s"FnDecl $name(${params.map(p => s"${p._1.value}: ${p._2.value}").mkString(", ")}) ${rettype.value}"
      )
      body.block.foreach(s => printNode(s, depth + 1))
    case VarDecl(const, WithSpan(name, _), tp, value) =>
      println(
        s"VarDecl ${if const then "val" else "var"} $name ${tp.value}"
      )
      printNode(value, depth + 1)
