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

type AST  = HashMap[Name, Decl]
type Name = String
enum Type {
  case I32
  case Bool
}

object Type:
  def fromString(s: String): Option[Type] =
    s match
      case "i32"  => Some(Type.I32)
      case "bool" => Some(Type.Bool)
      case _      => None

type Block = List[Stmt]

sealed case class FnDecl(
    val name: WithSpan[Name],
    val params: List[(WithSpan[Name], WithSpan[Type])],
    val rettype: Option[WithSpan[Type]],
    val body: BlockStmt
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
sealed case class IfStmt(
    val cond: Expr,
    val onTrue: BlockStmt,
    val onFalse: Option[BlockStmt | IfStmt]
) extends Stmt
sealed case class WhileStmt(
    val cond: Expr,
    val body: BlockStmt
) extends Stmt
sealed case class DeclStmt(val decl: Decl) extends Stmt
sealed case class RetStmt(val expr: Expr)  extends Stmt
case object VoidRetStmt                    extends Stmt

type Precedence = Int

enum UnaryOp {
  case Minus

  override def toString(): String =
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

  override def toString(): String =
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
}

sealed case class VarRefExpr(val name: WithSpan[Name]) extends Expr:
  override def toString(): String = s"${name.value}"

sealed case class CallExpr(val name: WithSpan[Name], val args: List[Expr]) extends Expr:
  override def toString: String = s"(${name.value} (${args.mkString(", ")}))"

sealed case class BinExpr(val op: WithSpan[BinOp], val lhs: Expr, val rhs: Expr) extends Expr:
  override def toString: String = s"($lhs ${op.value} $rhs)"

sealed case class UnaryExpr(val op: WithSpan[UnaryOp], val expr: Expr) extends Expr:
  override def toString: String = s"(${op.value} $expr)"

sealed case class NumLitExpr(val tp: Option[Type], val value: WithSpan[BigInt]) extends Expr:
  override def toString: String = value.value.toString

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
      println(s"NumLitExpr ${t.getOrElse("UNDEF")} $v")

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
    case IfStmt(cond, onTrue, onFalse) =>
      println("IfStmt")
      printExpr(cond, depth + 1)
      printStmt(onTrue, depth + 1)
      if onFalse.isDefined then printStmt(onFalse.get, depth + 1)
    case WhileStmt(cond, body) =>
      println("WhileStmt")
      printExpr(cond, depth + 1)
      printStmt(body, depth + 1)
    case VoidRetStmt => println("VoidRetStmt")

def printDecl(decl: Decl, depth: Int = 0): Unit =
  makeOffset(depth)
  decl match
    case FnDecl(WithSpan(name, _), params, rettype, body) =>
      println(
        s"FnDecl $name(${params.map(p => s"${p._1.value}: ${p._2.value}").mkString(", ")}) ${rettype.map(_.value).getOrElse("")}"
      )
      body.block.foreach(s => printStmt(s, depth + 1))
    case VarDecl(const, WithSpan(name, _), tp, value) =>
      println(
        s"VarDecl ${if const then "val" else "var"} $name ${tp.map(_.value).getOrElse("UNDEF")}"
      )
      printExpr(value, depth + 1)

def printAST(ast: AST): Unit =
  ast.foreachEntry((_, decl) => printDecl(decl))
