package frontend.ast

import scala.collection.mutable.HashMap
import frontend.lex.WithSpan

type TypeError = String

// sealed trait TypeInferrable:
//   def inferType: Type

// sealed trait TypeSettable[T]:
//   def setType(t: Type): Either[TypeError, T]

sealed trait AstNode
sealed trait Decl(val name: WithSpan[Name]) extends AstNode
sealed trait Stmt                           extends AstNode
sealed trait Expr                           extends AstNode

type AST   = HashMap[Name, Decl]
type Name  = String
type Type  = String
type Block = List[Stmt]

sealed class FnDecl(
    name: WithSpan[Name],
    val args: List[(WithSpan[Name], WithSpan[Type])],
    val rettype: WithSpan[Type],
    val body: Block
) extends Decl(name)

sealed class VarDecl(
    val const: Boolean,
    name: WithSpan[Name],
    val tp: Option[WithSpan[Type]],
    val value: Expr
) extends Decl(name)

sealed class ExprStmt(val expr: Expr)    extends Stmt
sealed class BlockStmt(val block: Block) extends Stmt
sealed class DeclStmt(val decl: Decl)    extends Stmt
sealed class RetStmt(val expr: Expr)     extends Stmt
sealed class VoidRetStmt                 extends Stmt

type Precedence = Int

enum UnaryOp:
  case Minus

enum BinOp extends Ordered[BinOp]:
  case Plus
  case Minus
  case Mul

  override def compare(that: BinOp): Int = this.precedence compare that.precedence
  def precedence: Precedence = this match
    case Plus  => 2
    case Minus => 2
    case Mul   => 3

sealed class VarRefExpr(val name: WithSpan[Name])                           extends Expr
sealed class CallExpr(val name: WithSpan[Name], val args: List[Expr])       extends Expr
sealed class BinExpr(val op: WithSpan[BinOp], val lhs: Expr, val rhs: Expr) extends Expr
sealed class UnaryExpr(val op: WithSpan[UnaryOp], val expr: Expr)           extends Expr
sealed class NumLitExpr(val tp: Type, val value: WithSpan[Int])             extends Expr
