package frontend.ast

import scala.collection.mutable.HashMap
import frontend.lex.WithSpan

type TypeError = String

// sealed trait TypeInferrable:
//   def inferType: Type

// sealed trait TypeSettable[T]:
//   def setType(t: Type): Either[TypeError, T]

sealed trait AstNode
sealed trait Decl extends AstNode:
  def getName: WithSpan[Name]
sealed trait Stmt extends AstNode
sealed trait Expr extends AstNode

type AST   = HashMap[Name, Decl]
type Name  = String
type Type  = String
type Block = List[Stmt]

sealed case class FnDecl(
    val name: WithSpan[Name],
    val params: List[(WithSpan[Name], WithSpan[Type])],
    val rettype: WithSpan[Type],
    val body: Block
) extends Decl:
  override def getName: WithSpan[Name] = name

sealed case class VarDecl(
    val const: Boolean,
    val name: WithSpan[Name],
    val tp: Option[WithSpan[Type]],
    val value: Expr
) extends Decl:
  override def getName: WithSpan[Name] = name

sealed case class ExprStmt(val expr: Expr)    extends Stmt
sealed case class BlockStmt(val block: Block) extends Stmt
sealed case class DeclStmt(val decl: Decl)    extends Stmt
sealed case class RetStmt(val expr: Expr)     extends Stmt
case object VoidRetStmt                       extends Stmt

type Precedence = Int

enum UnaryOp:
  case Minus

  override def toString(): String =
    this match
      case Minus => "-"

enum BinOp extends Ordered[BinOp]:
  case Plus
  case Minus
  case Mul

  override def toString(): String =
    this match
      case Plus  => "+"
      case Minus => "-"
      case Mul   => "*"

  override def compare(that: BinOp): Int = this.precedence compare that.precedence
  def precedence: Precedence = this match
    case Plus  => 2
    case Minus => 2
    case Mul   => 3

sealed case class VarRefExpr(val name: WithSpan[Name])                           extends Expr
sealed case class CallExpr(val name: WithSpan[Name], val args: List[Expr])       extends Expr
sealed case class BinExpr(val op: WithSpan[BinOp], val lhs: Expr, val rhs: Expr) extends Expr
sealed case class UnaryExpr(val op: WithSpan[UnaryOp], val expr: Expr)           extends Expr
sealed case class NumLitExpr(val tp: Type, val value: WithSpan[Int])             extends Expr

private def makeOffset(depth: Int) = print("  " * depth)

def printExpr(expr: Expr, depth: Int): Unit =
  makeOffset(depth)
  expr match
    case BinExpr(WithSpan(op, _), lhs, rhs) =>
      println(s"BinExpr $op")
      printExpr(lhs, depth + 1)
      printExpr(rhs, depth + 1)
    case VarRefExpr(WithSpan(name, _)) =>
      println(s"VarRefExpr $name")
    case CallExpr(WithSpan(name, _), args) =>
      println(s"CallExpr $name")
      args.foreach(e => printExpr(e, depth + 1))
    case UnaryExpr(WithSpan(op, _), e) =>
      println(s"UnaryExpr $op")
      printExpr(e, depth + 1)
    case NumLitExpr(t, WithSpan(v, _)) =>
      println(s"NumLitExpr $t $v")

def printStmt(stmt: Stmt, depth: Int): Unit =
  makeOffset(depth)
  stmt match
    case BlockStmt(block) =>
      println("BlockStmt")
      block.foreach(s => printStmt(s, depth + 1))
    case ExprStmt(e) =>
      println("ExprStmt")
      printExpr(e, depth + 1)
    case DeclStmt(d) =>
      println("DeclStmt")
      printDecl(d, depth + 1)
    case RetStmt(e) =>
      println("RetStmt")
      printExpr(e, depth + 1)
    case VoidRetStmt => println("VoidRetStmt")

def printDecl(decl: Decl, depth: Int = 0): Unit =
  makeOffset(depth)
  decl match
    case FnDecl(WithSpan(name, _), params, WithSpan(rettype, _), body) =>
      println(
        s"FnDecl $name(${params.map(p => s"${p._1.value}: ${p._2.value}").mkString(", ")}) $rettype"
      )
      body.foreach(s => printStmt(s, depth + 1))
    case VarDecl(const, WithSpan(name, _), tp, value) =>
      println(
        s"VarDecl ${if const then "val" else "var"} $name ${tp.map(_.value).getOrElse("UNDEF")}"
      )
      printExpr(value, depth + 1)

def printAST(ast: AST): Unit =
  ast.foreachEntry((_, decl) => printDecl(decl))
