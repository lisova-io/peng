package frontend.sema.passes

import frontend.sema.*
import frontend.translationUnit.TranslationUnit
import frontend.translationUnit.Name
import frontend.diagnostics.Diagnostic
import frontend.lex.WithSpan
import frontend.types.*
import frontend.ast.*

import scala.collection.immutable.HashSet
import scala.collection.MapView
import scala.collection.mutable.HashMap

object ArgsAmountCheckPass extends Pass[TranslationUnit, TranslationUnit] {
  private def check(e: Expr, ast: AST): List[Diagnostic] =
    e match
      case BinExpr(_, lhs, rhs, _) => check(lhs, ast) :++ check(rhs, ast)
      case VarRefExpr(_, _)        => Nil
      case UnaryExpr(_, e)         => check(e, ast)
      case NumLitExpr(_, _)        => Nil
      case BoolLitExpr(_)          => Nil
      case CallExpr(WithSpan(name, span), args) =>
        val fn: FnDecl = ast.get(name) match
          case Some(f: FnDecl) => f
          case None            => return Nil
          case _               => ??? // this should be unreachable for now
        val res =
          if args.length < fn.params.length then
            Diagnostic.error(
              span,
              s"too few arguments for call of function $name: expected ${fn.params.length}, but got ${args.length}",
            ) :: Nil
          else if args.length > fn.params.length then
            Diagnostic.error(
              span,
              s"too many arguments for call of function $name: expected ${fn.params.length}, but got ${args.length}",
            ) :: Nil
          else Nil
        res :++ args.foldLeft(Nil)((l, e) => l :++ check(e, ast))

  private def check(s: Stmt, ast: AST): List[Diagnostic] =
    s match
      case BlockStmt(b) => check(b, ast)
      case ExprStmt(e)  => check(e, ast)
      case IfStmt(cond, tBr, fBr) =>
        check(cond, ast) :++ check(tBr, ast) :++ fBr.map(check(_, ast)).getOrElse(Nil)
      case WhileStmt(cond, body) => check(cond, ast) :++ check(body.block, ast)
      case DeclStmt(d)           => check(d, ast)
      case RetStmt(e)            => check(e, ast)
      case UnitRetStmt(_)        => Nil

  private def check(b: Block, ast: AST): List[Diagnostic] =
    b.foldLeft(Nil)((d, s) => d :++ check(s, ast))

  private def check(d: Decl, ast: AST): List[Diagnostic] =
    d match
      case FnDecl(name, params, rettype, body) => check(body.block, ast)
      case VarDecl(const, name, tp, value)     => check(value, ast)

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] = {
    val diagnostics: List[Diagnostic] =
      unit.ast.foldLeft(Nil)((diags, decl) => diags :++ check(decl._2, unit.ast))
    SemaResult(unit, diagnostics)
  }
}
