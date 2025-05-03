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

object TypePropagationPass extends Pass[TranslationUnit, TranslationUnit] {
  private class Ctx(
      val fns: HashMap[Name, FnDecl] = HashMap(),
      val vars: HashMap[Name, Type] = HashMap(),
  ):
    def getFn(name: Name): Option[FnDecl]    = fns.get(name)
    def getVarType(name: Name): Option[Type] = vars.get(name)
    def addVar(n: Name, t: Type): Ctx        = Ctx(fns, vars + (n -> t))
    def addVars(vs: Seq[(Name, Type)]): Ctx  = Ctx(fns, vars ++ vs)

  private def propagateType(ifStmt: IfStmt, tp: Type, ctx: Ctx): IfStmt = {
    val IfStmt(c, tBr, fBr) = ifStmt
    IfStmt(
      propagateType(propagateTypes(c, ctx), Type.Bool, ctx),
      BlockStmt(propagateType(tBr.block, tp, ctx)),
      fBr.map(s =>
        s match
          case b: BlockStmt => BlockStmt(propagateType(b.block, tp, ctx))
          case ifs: IfStmt  => propagateType(ifs, tp, ctx)
      ),
    )
  }

  private def propagateType(block: Block, tp: Type, _ctx: Ctx): Block =
    var newBlock: Block = Nil
    var ctx             = _ctx
    for s <- block yield s match
      case b: BlockStmt => BlockStmt(propagateType(b.block, tp, ctx))
      case ExprStmt(e)  => ExprStmt(propagateTypes(e, ctx))
      case s: IfStmt    => propagateType(s, tp, ctx)
      case WhileStmt(c, body) =>
        WhileStmt(propagateType(c, Type.Bool, ctx), BlockStmt(propagateType(body.block, tp, ctx)))
      case DeclStmt(d) =>
        val res = DeclStmt(propagateTypes(d, ctx))
        ctx = ctx.addVar(d.name.value, d.tp.value)
        res
      case RetStmt(e)     => RetStmt(propagateType(e, tp, ctx))
      case u: UnitRetStmt => u

  private def propagateTypes(e: Expr, ctx: Ctx): Expr =
    e match
      case BinExpr(op, lhs, rhs, tp) =>
        BinExpr(op, propagateTypes(lhs, ctx), propagateTypes(rhs, ctx), tp)
      case VarRefExpr(name, vtp) => VarRefExpr(name, ctx.getVarType(name.value).get)
      case CallExpr(name, args) => {
        val FnDecl(_, params, rettype, body) = ctx.getFn(name.value).get
        val newArgs = args
          .zip(params)
          .map(arg => {
            val (e, (_, t)) = arg
            propagateType(propagateTypes(e, ctx), t.value, ctx)
          })
        CallExpr(name, newArgs)
      }
      case UnaryExpr(op, e) => UnaryExpr(op, propagateTypes(e, ctx))
      case n: NumLitExpr    => n
      case b: BoolLitExpr   => b

  private def propagateType(e: Expr, tp: Type, ctx: Ctx): Expr =
    propagateTypes(e, ctx) match
      case BinExpr(op, lhs, rhs, t) =>
        BinExpr(
          op,
          propagateType(lhs, tp, ctx),
          propagateType(rhs, tp, ctx),
          if !op.value.isCmp then t else Type.Bool,
        )
      case vre: VarRefExpr => vre
      case CallExpr(name, args) => {
        ctx.getFn(name.value) match
          case Some(FnDecl(_, params, rettype, body)) =>
            val newArgs = args
              .zip(params)
              .map(arg => {
                val (e, (_, t)) = arg
                propagateType(e, t.value, ctx)
              })
            CallExpr(name, newArgs)
          case None => CallExpr(name, args)
      }
      case UnaryExpr(op, e)         => UnaryExpr(op, propagateType(e, tp, ctx))
      case n @ NumLitExpr(_, value) => if tp.isIntegerType then NumLitExpr(tp, value) else n
      case BoolLitExpr(b)           => BoolLitExpr(b)

  private def propagateTypes(fn: FnDecl, ctx: Ctx): FnDecl =
    val FnDecl(n, p, r, body) = fn
    val newCtx                = ctx.addVars(p.map(param => param._1.value -> param._2.value))
    FnDecl(n, p, r, BlockStmt(propagateType(body.block, r.value, newCtx)))

  private def propagateTypes(vard: VarDecl, ctx: Ctx): VarDecl =
    val VarDecl(c, n, t, value) = vard
    t match
      case ws @ WithSpan(Type.Undef, s) => VarDecl(c, n, ws, propagateTypes(value, ctx))
      case ws @ WithSpan(t, s) =>
        VarDecl(c, n, ws, propagateType(value, t, ctx))

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] = {
    val ctx = Ctx(
      unit.ast
        .filter(_._2 match
          case f: FnDecl => true
          case _         => false)
        .asInstanceOf[HashMap[Name, FnDecl]],
      HashMap.from(
        unit.ast
          .filter(_._2 match
            case v: VarDecl => true
            case _          => false)
          .mapValues(_.asInstanceOf[VarDecl].tp.value)
      ),
    )
    unit.ast.mapValuesInPlace((_, d) =>
      d match
        case fnd: FnDecl   => propagateTypes(fnd, ctx)
        case vard: VarDecl => propagateTypes(vard, ctx)
    )
    SemaResult(unit)
  }
}
