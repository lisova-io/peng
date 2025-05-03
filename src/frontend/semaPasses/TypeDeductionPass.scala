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

private def coerceTypes(lhs: Type, rhs: Type): Type =
  (lhs, rhs) match
    case (x, y) if x == y => x
    case (x, Type.Undef)  => x
    case (Type.Undef, y)  => y
    case _                => Type.Invalid

object TypeDeductionPass extends Pass[TranslationUnit, TranslationUnit] {
  private type Ctx = HashMap[Name, Type]

  private def deduceTypes(d: VarDecl, ctx: Ctx): SemaResult[VarDecl] =
    val VarDecl(c, n, tp, value) = d
    tp.value match
      case Type.Undef =>
        deduceTypes(value, ctx).map(res => VarDecl(c, n, WithSpan(res._2, res._1.getSpan), res._1))
      case _ => deduceTypes(value, ctx).map(r => VarDecl(c, n, tp, r._1))

  private def deduceTypes(d: Decl, ctx: Ctx): SemaResult[Decl] =
    d match
      case FnDecl(n, p, r, body) =>
        deduceTypes(
          body.block,
          ctx + (n.value -> r.value) ++ p.map(arg => arg._1.value -> arg._2.value),
        ).map(b => FnDecl(n, p, r, BlockStmt(b)))
      case d: VarDecl => deduceTypes(d, ctx)

  private def deduceTypes(s: IfStmt, ctx: Ctx): SemaResult[IfStmt] =
    val IfStmt(cond, tBr, fBr) = s
    for {
      c <- deduceTypes(cond, ctx)
      t <- deduceTypes(tBr.block, ctx).map(BlockStmt(_))
      f <- fBr
        .map(
          _ match
            case BlockStmt(block) => deduceTypes(block, ctx).map(b => Some(BlockStmt(b)))
            case ifs: IfStmt      => deduceTypes(ifs, ctx).map(Some(_))
        )
        .getOrElse(SemaResult(None))
    } yield IfStmt(c._1, t, f)

  private def deduceTypes(s: Stmt, ctx: Ctx): SemaResult[(Stmt, Ctx)] =
    s match
      case BlockStmt(block) => deduceTypes(block, ctx).map(b => (BlockStmt(b), ctx))
      case ifs: IfStmt      => deduceTypes(ifs, ctx).map(ifs => (ifs, ctx))
      case ExprStmt(e)      => deduceTypes(e, ctx).map(e => (ExprStmt(e._1), ctx))
      case WhileStmt(cond, body) =>
        for {
          (c, _) <- deduceTypes(cond, ctx)
          b      <- deduceTypes(body.block, ctx)
        } yield (WhileStmt(c, BlockStmt(b)), ctx)
      case DeclStmt(d) =>
        deduceTypes(d, ctx).map(d => (DeclStmt(d), ctx + (d.name.value -> d.tp.value)))
      case RetStmt(e)     => deduceTypes(e, ctx).map(e => (RetStmt(e._1), ctx))
      case u: UnitRetStmt => SemaResult((u, ctx))

  private def deduceTypes(b: Block, ctx: Ctx): SemaResult[Block] =
    val initial = SemaResult((List[Stmt](), ctx))
    b.foldLeft(initial)((res, stmt) =>
      res.flatMap(res => {
        val (b, ctx1) = res
        deduceTypes(stmt, ctx1).map(res => (b :+ res._1, res._2))
      })
    ).map(_._1)

  private def deduceTypes(e: Expr, ctx: Ctx): SemaResult[(Expr, Type)] =
    e match
      case BinExpr(op, lhs, rhs, tp) =>
        for {
          (le, lt) <- deduceTypes(lhs, ctx)
          (re, rt) <- deduceTypes(rhs, ctx)
          t =
            if op.value.isCmp then Type.Bool
            else coerceTypes(tp, coerceTypes(lt, rt))
        } yield (BinExpr(op, le, re, t), t)
      case VarRefExpr(name, Type.Undef) =>
        val vtp = ctx.get(name.value).get
        SemaResult((VarRefExpr(name, vtp), vtp))
      case vre @ VarRefExpr(name, vtp) => SemaResult((vre, vtp))
      case CallExpr(name, args) =>
        var diagnostics: List[Diagnostic] = Nil
        val newArgs = args.map(e =>
          val SemaResult(e2, d) = deduceTypes(e, ctx)
          diagnostics :++= d
          e2._1
        )
        SemaResult((CallExpr(name, newArgs), ctx.getOrElse(name.value, Type.Invalid)), diagnostics)
      case UnaryExpr(op, e)      => deduceTypes(e, ctx).map(res => (UnaryExpr(op, res._1), res._2))
      case n @ NumLitExpr(tp, _) => SemaResult((n, tp))
      case b: BoolLitExpr        => SemaResult((b, Type.Bool))

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] =
    var diagnostics: List[Diagnostic] = Nil
    var ctx: Ctx                      = HashMap.from(unit.ast.mapValues(_.getType.value))
    unit.ast.mapValuesInPlace((n, d) => {
      val SemaResult(newDecl, diags) = deduceTypes(d, ctx)
      ctx(n) = newDecl.getType.value
      newDecl
    })
    SemaResult(unit, diagnostics)
}
