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
import frontend.diagnostics.Message

object TypeCheckPass extends Pass[TranslationUnit, TranslationUnit] {
  private type Ctx = HashMap[Name, (Type, Option[Decl])]

  private def checkTypes(lhs: Type, rhs: Type): Boolean =
    (lhs, rhs) match
      case (Type.Undef, _) | (_, Type.Undef)     => ??? // UNDEF IS NOT WELCOME HERE
      case (Type.Invalid, _) | (_, Type.Invalid) => true
      case (a, b)                                => a == b

  private def typeCheck(d: Decl, ctx: Ctx): List[Diagnostic] =
    d match
      case FnDecl(name, params, rettype, body) =>
        typeCheck(
          body.block,
          rettype,
          ctx ++ params.map(p => p._1.value -> (p._2.value -> None)),
        )
      case VarDecl(const, name, tp, value) =>
        val (valtp, diags) = typeCheck(value, None, ctx)
        if checkTypes(valtp, tp.value) then diags
        else
          diags :+ Diagnostic.error(
            Message(
              value.getSpan,
              s"variable initializer type mismatch: expected ${tp.value}, but got $valtp",
            ) :: Message(tp.span, "vatiable type defined here") :: Nil
          )

  private def typeCheck(ifs: IfStmt, tp: WithSpan[Type], ctx: Ctx): List[Diagnostic] =
    val IfStmt(cond, tBr, fBr) = ifs
    val (_, d1)                = typeCheck(cond, Some(Type.Bool), ctx)
    val d2                     = typeCheck(tBr.block, tp, ctx)
    val d3 = fBr
      .map(_ match
        case BlockStmt(block) => typeCheck(block, tp, ctx)
        case ifs: IfStmt      => typeCheck(ifs, tp, ctx))
      .getOrElse(Nil)
    d1 :++ d2 :++ d3

  private def typeCheck(b: Block, tp: WithSpan[Type], ctx: Ctx): List[Diagnostic] =
    b.foldLeft((ctx, List[Diagnostic]()))((ctxAndD, s) => {
      val (ctx, diags) = ctxAndD
      s match
        case BlockStmt(block) => (ctx, diags :++ typeCheck(block, tp, ctx))
        case ExprStmt(e)      => (ctx, diags :++ typeCheck(e, None, ctx)._2)
        case ifs: IfStmt      => (ctx, diags :++ typeCheck(ifs, tp, ctx))
        case WhileStmt(cond, body) =>
          val (_, d1) = typeCheck(cond, Some(Type.Bool), ctx)
          val d2      = typeCheck(body.block, tp, ctx)
          ctx -> (d1 :++ d2)
        case DeclStmt(d) =>
          (ctx + (d.name.value -> (d.tp.value -> Some(d))), diags :++ typeCheck(d, ctx))
        case RetStmt(e) =>
          val (t, d1) = typeCheck(e, Some(tp.value), ctx)
          val d2 =
            if checkTypes(t, tp.value)
            then Nil
            else
              Diagnostic.error(
                Message(
                  e.getSpan,
                  s"function return type mismatch: expected ${tp.value}, but got ${t}",
                ) ::
                  Message(tp.span, "function return type defined here") ::
                  Nil
              ) :: Nil
          ctx -> (diags :++ d1 :++ d2)
        case UnitRetStmt(s) =>
          val d =
            if tp.value == Type.Unit
            then Nil
            else
              Diagnostic.error(
                Message(
                  s,
                  s"function return type mismatch: expected ${tp.value}, but got ${Type.Unit}",
                ) ::
                  Message(tp.span, "function return type defined here") ::
                  Nil
              ) :: Nil
          ctx -> (diags :++ d)
    })._2

  private def typeCheck(e: Expr, tp: Option[Type], ctx: Ctx): (Type, List[Diagnostic]) =
    e match
      case BinExpr(op, lhs, rhs, bt) =>
        val expectedType =
          if op.value.isCmp || tp.filter(_ == Type.Invalid).isDefined then None
          else if bt != Type.Invalid then Some(bt)
          else tp
        val (t1, d1) = typeCheck(lhs, expectedType, ctx)
        val (t2, d2) = typeCheck(rhs, expectedType, ctx)
        var d        = d1 :++ d2
        val t = if !checkTypes(t1, t2) then {
          d :+= Diagnostic.error(
            op.span,
            s"operand types mismatch: left is $t1, right is $t2",
          )
          Type.Invalid
        } else if t1 == Type.Invalid || t2 == Type.Invalid then Type.Invalid
        else t1
        val opCheckDiags =
          if tp.isDefined && op.value.isCmp && !checkTypes(tp.get, Type.Bool)
          then
            Diagnostic.error(
              op.span,
              s"type mismatch: expected ${tp.get}, but got ${Type.Bool}",
            ) :: Nil
          else Nil
        val opTp =
          if op.value.isCmp && opCheckDiags.isEmpty then Type.Bool
          else if op.value.isCmp then Type.Invalid
          else t
        assert(opTp == bt)
        (opTp, d :++ opCheckDiags)
      case VarRefExpr(name, vtp) =>
        if tp.isEmpty || checkTypes(tp.get, vtp)
        then (vtp, Nil)
        else
          Type.Invalid -> (Diagnostic.error(
            name.span,
            s"type mismatch: expected ${tp.get}, got ${vtp}",
          ) :: Nil)
      case CallExpr(name, args) =>
        val fn: FnDecl = ctx.get(name.value) match
          case Some(_, Some(fn: FnDecl)) => fn
          case None                      => return (Type.Invalid, Nil)
          case _                         => ??? // this should not be reachable
        fn.rettype.value ->
          args
            .zip(fn.params)
            .map(arg => {
              val (e, (name, tp)) = arg
              typeCheck(e, Some(tp.value), ctx)._2
            })
            .fold(Nil)(_ :++ _)
      case UnaryExpr(op, e) => typeCheck(e, tp, ctx)
      case NumLitExpr(t, value) =>
        if tp.isEmpty || checkTypes(t, tp.get) then (t, Nil)
        else
          (
            Type.Invalid,
            Diagnostic.error(value.span, s"type mismatch: expected ${tp.get}, but got $t") :: Nil,
          )
      case BoolLitExpr(value) =>
        if tp.isEmpty || checkTypes(Type.Bool, tp.get) then (Type.Bool, Nil)
        else
          Type.Invalid -> (Diagnostic.error(
            value.span,
            s"type mismatch: expected ${tp.get}, but got ${Type.Bool}",
          ) :: Nil)

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] =
    var ctx: Ctx = HashMap.from(unit.ast.mapValues(d => (d.getType.value, Some(d))))
    val diagnostics = unit.ast
      .map(p => {
        val (n, d) = p
        typeCheck(d, ctx)
      })
      .fold(Nil)(_ :++ _)
    SemaResult(unit, diagnostics)
}
