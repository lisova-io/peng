package frontend.ast

sealed trait AstNode
sealed trait Expr
sealed trait Stmt
sealed trait Decl

type Name  = String
type Type  = String
type Block = List[Stmt]

class FnArg(val name: Name, val tp: Type)
class FnDecl(val args: List[FnArg], val tp: Type, val body: Block) extends Decl
