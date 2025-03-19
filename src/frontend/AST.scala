package frontend.ast

import scala.collection.mutable.HashMap

type TypeError = String

// sealed trait TypeInferrable:
//   def inferType: Type

// sealed trait TypeSettable[T]:
//   def setType(t: Type): Either[TypeError, T]

sealed trait AstNode
sealed trait Decl extends AstNode:
  def getName: Name
sealed trait Stmt extends AstNode
sealed trait Expr extends AstNode

type AST   = HashMap[Name, Decl]
type Name  = String
type Type  = String
type Block = List[Stmt]

sealed class FnDecl(val name: Name, val args: List[(Name, Type)], val tp: Type, val body: Block)
    extends Decl {
  override def getName: Name = name
}

sealed class VarDecl(val const: Boolean, val name: Name, val tp: Option[Type], val value: Expr)
    extends Decl {
  override def getName: Name = name
}

sealed class ExprStmt(val expr: Expr)        extends Stmt
sealed class BlockStmt(val block: Block)     extends Stmt
sealed class DeclStmt(val decl: Decl)        extends Stmt
sealed class RetStmt(val expr: Option[Expr]) extends Stmt
sealed class VoidRetStmt                     extends Stmt

enum UnaryOp:
  case Minus

enum BinOp:
  case Plus
  case Minus

sealed class VarRefExpr(val name: Name)                           extends Expr
sealed class CallExpr(val name: Name, val args: List[Expr])       extends Expr
sealed class BinExpr(val op: BinOp, val lhs: Expr, val rhs: Expr) extends Expr
sealed class UnaryExpr(val op: UnaryOp, val expr: Expr)           extends Expr
sealed class NumLitExpr(val tp: Type, val name: Name)             extends Expr
