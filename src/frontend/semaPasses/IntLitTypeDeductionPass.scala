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

private def deduceIntType(v: BigInt): Option[Type.I32.type | Type.I64.type | Type.U64.type] = {
  def inRange(x: BigInt, r: (BigInt, BigInt)): Boolean = r._1 <= x && x <= r._2
  def range(bits: Int, signed: Boolean): (BigInt, BigInt) =
    if signed
    then -((BigInt(1) << (bits - 1))) -> ((BigInt(1) << (bits - 1)) - 1)
    else BigInt(0)                    -> ((BigInt(1) << bits) - 1)

  v match
    // case i8 if inRange(i8, range(8, signed = true))     => Some(Type.I8)
    // case u8 if inRange(u8, range(8, signed = false))    => Some(Type.U8)
    // case i16 if inRange(i16, range(16, signed = true))  => Some(Type.I16)
    // case u16 if inRange(u16, range(16, signed = false)) => Some(Type.U16)
    case i32 if inRange(i32, range(32, signed = true)) => Some(Type.I32)
    // case u32 if inRange(u32, range(32, signed = false)) => Some(Type.U32)
    case i64 if inRange(i64, range(64, signed = true))  => Some(Type.I64)
    case u64 if inRange(u64, range(64, signed = false)) => Some(Type.U64)
    case _                                              => None
}

object IntLitTypeDeductionPass extends Pass[TranslationUnit, TranslationUnit] {
  private def deduceIntTypes(d: VarDecl): SemaResult[VarDecl] =
    val VarDecl(c, n, tp, value) = d
    deduceIntTypes(value).map(e => VarDecl(c, n, tp, e))

  private def deduceIntTypes(d: Decl): SemaResult[Decl] =
    d match
      case FnDecl(n, p, r, body) =>
        deduceIntTypes(body.block).map(b => FnDecl(n, p, r, BlockStmt(b)))
      case d: VarDecl => deduceIntTypes(d)

  private def deduceIntTypes(s: IfStmt): SemaResult[IfStmt] =
    val IfStmt(cond, tBr, fBr) = s
    for {
      c <- deduceIntTypes(cond)
      t <- deduceIntTypes(tBr.block).map(BlockStmt(_))
      f <- fBr
        .map(
          _ match
            case BlockStmt(block) => deduceIntTypes(block).map(b => Some(BlockStmt(b)))
            case ifs: IfStmt      => deduceIntTypes(ifs).map(Some(_))
        )
        .getOrElse(SemaResult(None))
    } yield IfStmt(c, t, f)

  private def deduceIntTypes(s: Stmt): SemaResult[Stmt] =
    s match
      case BlockStmt(block) => deduceIntTypes(block).map(BlockStmt(_))
      case ifs: IfStmt      => deduceIntTypes(ifs)
      case ExprStmt(e)      => deduceIntTypes(e).map(ExprStmt(_))
      case WhileStmt(cond, body) =>
        for {
          c <- deduceIntTypes(cond)
          b <- deduceIntTypes(body.block)
        } yield WhileStmt(c, BlockStmt(b))
      case DeclStmt(d)    => deduceIntTypes(d).map(DeclStmt(_))
      case RetStmt(e)     => deduceIntTypes(e).map(RetStmt(_))
      case u: UnitRetStmt => SemaResult(u)

  private def deduceIntTypes(b: Block): SemaResult[Block] =
    val initial = SemaResult(List[Stmt]())
    b.foldLeft(initial)((res, stmt) => res.flatMap(b => deduceIntTypes(stmt).map(b :+ _)))

  private def deduceIntTypes(e: Expr): SemaResult[Expr] =
    e match
      case BinExpr(op, lhs, rhs, tp) =>
        for {
          l <- deduceIntTypes(lhs)
          r <- deduceIntTypes(rhs)
        } yield BinExpr(op, l, r, tp)
      case vre: VarRefExpr => SemaResult(vre)
      case CallExpr(n, args) =>
        var diagnostics: List[Diagnostic] = Nil
        val newArgs = args.map(e =>
          val SemaResult(e2, d) = deduceIntTypes(e)
          diagnostics :++= d
          e2
        )
        SemaResult(CallExpr(n, newArgs), diagnostics)
      case UnaryExpr(op, e) => deduceIntTypes(e).map(UnaryExpr(op, _))
      case n @ NumLitExpr(tp, ws @ WithSpan(value, span)) =>
        if tp == Type.Undef then
          deduceIntType(value)
            .map(t => SemaResult(NumLitExpr(t, ws)))
            .getOrElse(
              SemaResult(
                NumLitExpr(Type.Invalid, ws),
                Diagnostic
                  .error(span, s"value $value is out of bounds of every integer type") :: Nil,
              )
            )
        else SemaResult(n)
      case b: BoolLitExpr => SemaResult(b)

  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] =
    var diagnostics: List[Diagnostic] = Nil
    unit.ast.mapValuesInPlace((name, decl) => {
      val SemaResult(d, diags) = deduceIntTypes(decl)
      diagnostics :++= diags
      d
    })
    SemaResult(unit, diagnostics)
}
