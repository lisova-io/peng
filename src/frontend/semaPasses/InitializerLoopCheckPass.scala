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

object InitializerLoopCheckPass extends Pass[TranslationUnit, TranslationUnit] {
  private class Ctx(val globals: AST, val visited: HashSet[Name]):
    def visit(n: Name): Ctx  = Ctx(globals, visited + n)
    def shadow(n: Name): Ctx = Ctx(globals - n, visited)

  private def getDeps(n: AstNode, ctx: Ctx): (HashSet[Name], Ctx) =
    n match
      case FnDecl(name, params, _, body) => {
        if ctx.visited.contains(name.value) then (HashSet(), ctx)
        else
          getDeps(body, params.foldLeft(ctx.visit(name.value))((ctx, p) => ctx.shadow(p._1.value)))
      }
      case d @ VarDecl(_, name, _, v) =>
        val isGlobal = ctx.globals.contains(name.value) && ctx.globals(name.value) == d
        if isGlobal && ctx.visited.contains(name.value) then (HashSet(), ctx)
        else getDeps(v, if isGlobal then ctx.visit(name.value) else ctx.shadow(name.value))
      case BlockStmt(block) => {
        val (deps, _) = block.foldLeft((HashSet[Name](), ctx))((res, stmt) => {
          val (deps, ctx)       = res
          val (newDeps, newCtx) = getDeps(stmt, ctx)
          (deps ++ newDeps, newCtx)
        })
        (deps, ctx)
      }
      case IfStmt(cond, tBr, fBr) =>
        (
          getDeps(cond, ctx)._1 ++
            getDeps(tBr, ctx)._1 ++
            fBr
              .map(getDeps(_, ctx)._1)
              .getOrElse(HashSet()),
          ctx,
        )
      case WhileStmt(cond, body) =>
        (
          getDeps(cond, ctx)._1 ++
            getDeps(body, ctx)._1,
          ctx,
        )
      case ExprStmt(e)             => getDeps(e, ctx)
      case DeclStmt(d)             => getDeps(d, ctx)
      case RetStmt(e)              => getDeps(e, ctx)
      case UnitRetStmt(_)          => (HashSet(), ctx)
      case BinExpr(_, lhs, rhs, _) => (getDeps(lhs, ctx)._1 ++ getDeps(rhs, ctx)._1, ctx)
      case UnaryExpr(_, e)         => getDeps(e, ctx)
      case VarRefExpr(name, _) =>
        (
          ctx.globals
            .get(name.value)
            .map(d => getDeps(d, ctx)._1)
            .getOrElse(HashSet()) + name.value,
          ctx,
        )
      case CallExpr(name, args) =>
        (
          ctx.globals.get(name.value).map(d => getDeps(d, ctx)._1).getOrElse(HashSet())
            ++ args.foldLeft(HashSet[Name]())((deps, e) => deps ++ getDeps(e, ctx)._1)
            + name.value,
          ctx,
        )
      case NumLitExpr(_, _) | BoolLitExpr(_) => (HashSet(), ctx)

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] = {
    val deps: MapView[Name, HashSet[Name]] =
      unit.ast.mapValues(getDeps(_, Ctx(unit.ast, HashSet()))._1)

    var diagnostics = List[Diagnostic]()
    var newAst: AST = HashMap()
    var badNames    = HashSet[Name]()
    for (name, decl) <- unit.ast do {
      decl match
        case d: FnDecl => newAst += (name, decl)
        case VarDecl(_, name, _, _) if deps(name.value).contains(name.value) =>
          diagnostics :+= Diagnostic.error(name.span, "dependency loop in initializer")
          badNames += name.value
        case VarDecl(_, _, _, _) => newAst += (name, decl)
    }

    SemaResult(
      TranslationUnit(unit.types, newAst.filter(p => deps(p._1).intersect(badNames).isEmpty)),
      diagnostics,
    )
  }
}
