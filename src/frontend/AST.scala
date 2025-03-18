package frontend.ast

import scala.collection.immutable.HashMap

sealed trait TypeInferrable:
  def inferType: Type

sealed trait TypeSettable[T]:
  def setType(t: Type): Either[String, T]

sealed trait AstNode
sealed trait Expr extends AstNode with TypeInferrable with TypeSettable[Expr]
sealed trait Stmt extends AstNode
sealed trait Decl extends AstNode:
  def name: Name

type AST  = HashMap[Name, Decl]
type Name = String
type Type = String
class Block(repr: List[Stmt]) extends TypeInferrable with TypeSettable[Block]:
  override def inferType: Type                         = ???
  override def setType(t: Type): Either[String, Block] = ???

class FnDecl(val name: Name, val args: List[(Name, Type)], val tp: Type, val body: Block)
    extends Decl
class VarDecl(val name: Name, val tp: Option[Type], val value: Expr) extends Decl

class ExprStmt(val expr: Expr)        extends Stmt
class BlockStmt(val block: Block)     extends Stmt
class DeclStmt(val decl: Decl)        extends Stmt
class RetStmt(val expr: Option[Expr]) extends Stmt
class VoidRetStmt                     extends Stmt

enum UnaryOp:
  case Minus

enum BinOp:
  case Plus
  case Minus

class VarRefExpr(val name: Name) extends Expr:
  override def inferType: Type                              = ???
  override def setType(t: Type): Either[String, VarRefExpr] = ???
class CallExpr(val name: Name, val args: List[Expr]) extends Expr:
  override def inferType: Type                            = ???
  override def setType(t: Type): Either[String, CallExpr] = ???
class BinExpr(val op: BinOp, val lhs: Expr, val rhs: Expr) extends Expr:
  override def inferType: Type                           = ???
  override def setType(t: Type): Either[String, BinExpr] = ???
class UnaryExpr(val op: UnaryOp, val expr: Expr) extends Expr:
  override def inferType: Type                             = ???
  override def setType(t: Type): Either[String, UnaryExpr] = ???
class NumLitExpr(val tp: Type, val name: Name) extends Expr:
  override def inferType: Type                              = ???
  override def setType(t: Type): Either[String, NumLitExpr] = ???
