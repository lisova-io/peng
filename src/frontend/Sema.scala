package frontend.sema

import frontend.ast._
import frontend.diagnostics.Diagnostic
import scala.collection.mutable.HashMap

type SemaResult[T] = (Option[T], List[Diagnostic])

class Sema:
  type Ctx = HashMap[Name, Type]

  def process(ast: AST): (AST, List[Diagnostic]) = {
    var res @ (resAst, resDiagnostics) =
      (
        ast.map(p => {
          val (name, decl) = p
          (name, decl.getType)
        }),
        List[Diagnostic]()
      )
    for ((name, d) <- ast) {
      val (decl, diags) = processDecl(d, resAst)
      resDiagnostics ++= diags
      if decl.isDefined then resAst(name) = decl.get
    }

    res
  }

  def processDecl(d: Decl, ctx: Ctx): SemaResult[Decl] =
    d match
      case FnDecl(name, params, rettype, body) =>
        ???
        processBlock(body, ctx ++ params)
      case VarDecl(const, name, tp, value) => ???

  def processBlock(
      b: Block,
      ctx: Ctx,
      expectedType: Option[Type] = None
  ): SemaResult[Block] = ???

  def processStmt(s: Stmt, ctx: Ctx): SemaResult[Stmt] = ???

  def processExpr(e: Expr, ctx: Ctx): SemaResult[Expr] = ???
