package frontend.sema.passes

import frontend.sema.*
import frontend.translationUnit.TranslationUnit
import frontend.translationUnit.Name
import frontend.diagnostics.Diagnostic
import frontend.lex.WithSpan
import frontend.types.*
import frontend.ast.*

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object AssignmentCorrectnessCheckPass extends Pass[TranslationUnit, TranslationUnit] {
  private class ConstVars(private var constVars: HashSet[Name] = HashSet()) {
    def addOne(d: Name): ConstVars   = ConstVars(constVars += d)
    def `+`                          = addOne
    def isConst(name: Name): Boolean = constVars.contains(name)
  }

  private object ConstVars {
    def fromAST(ast: AST): ConstVars = {
      var res = ConstVars()
      for (n, d) <- ast do
        d match
          case vd: VarDecl if vd.const => res += n
          case _                       => ()
      res
    }
  }

  private def check(e: Expr, ctx: ConstVars): List[Diagnostic] =
    e match
      case BinExpr(WithSpan(BinOp.Assign, span), lhs, rhs, _) =>
        lhs match
          case VarRefExpr(name, _) if ctx.isConst(name.value) =>
            Diagnostic.error(
              name.span,
              s"cannot assign a new value to a non-mutable variable",
            ) :: Nil
          case v: VarRefExpr => Nil
          case _ => Diagnostic.error(e.getSpan, s"cannot assign to this expression") :: Nil
      case BinExpr(_, lhs, rhs, _) => check(lhs, ctx) :++ check(rhs, ctx)
      case VarRefExpr(_, _)        => Nil
      case UnaryExpr(_, e)         => check(e, ctx)
      case NumLitExpr(_, _)        => Nil
      case BoolLitExpr(_)          => Nil
      case CallExpr(_, args)       => args.map(check(_, ctx)).fold(Nil)(_ :++ _)

  private def check(s: Stmt, ctx: ConstVars): List[Diagnostic] =
    s match
      case BlockStmt(b) => check(b, ctx)
      case ExprStmt(e)  => check(e, ctx)
      case IfStmt(cond, tBr, fBr) =>
        check(cond, ctx) :++ check(tBr, ctx) :++ fBr.map(check(_, ctx)).getOrElse(Nil)
      case WhileStmt(cond, body) => check(cond, ctx) :++ check(body.block, ctx)
      case DeclStmt(d)           => check(d, ctx)
      case RetStmt(e)            => check(e, ctx)
      case UnitRetStmt(_)        => Nil

  private def check(b: Block, _ctx: ConstVars): List[Diagnostic] =
    var ctx                     = _ctx
    var diags: List[Diagnostic] = Nil
    for s <- b do
      diags :++= check(s, ctx)
      s match
        case DeclStmt(d: VarDecl) if d.const => ctx += d.name.value
        case _                               => ()
    diags

  private def check(d: Decl, ctx: ConstVars): List[Diagnostic] =
    d match
      case FnDecl(name, params, rettype, body) => check(body.block, ctx)
      case VarDecl(const, name, tp, value)     => check(value, ctx)

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] = {
    var ctx = ConstVars.fromAST(unit.ast)
    SemaResult(unit, unit.ast.foldLeft(Nil)((diags, decl) => diags :++ check(decl._2, ctx)))
  }
}
