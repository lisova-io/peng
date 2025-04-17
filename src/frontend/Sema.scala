package frontend.sema

import frontend.ast.*
import frontend.diagnostics.{Diagnostic, Message}
import frontend.lex.{Span, WithSpan}

import scala.collection.mutable.{HashMap, HashSet}
import scala.collection.MapView

import diagnostics.containsErrors

class SemaResult[+T](val value: T, val diagnostics: List[Diagnostic]) {
  def this(v: T) = this(v, Nil)
  def map[U](fn: T => U): SemaResult[U] = SemaResult(fn(value), diagnostics)
  def flatMap[U](fn: T => SemaResult[U]): SemaResult[U] = {
    val SemaResult(v, d) = fn(value)
    SemaResult(v, diagnostics :++ d)
  }
  def foreach[U](fn: T => U): Unit = {
    fn(value)
    ()
  }
  def andThen[U](that: => SemaResult[U]): SemaResult[U] = {
    val SemaResult(v, d) = that
    SemaResult(v, diagnostics :++ d)
  }
}

object SemaResult:
  def unapply[T](r: SemaResult[T]): (T, List[Diagnostic]) = (r.value, r.diagnostics)

private trait Pass[T, U]:
  def run(arg: T): SemaResult[U]
  final infix def compose[V](next: Pass[U, V]): Pass[T, V] = PassComposition(this, next)
  final infix def `>>`[V](next: Pass[U, V]): Pass[T, V]    = PassComposition(this, next)

private class PassComposition[T, U, V](first: Pass[T, U], second: Pass[U, V]) extends Pass[T, V] {
  override def run(arg: T): SemaResult[V] = {
    val SemaResult(r1, d1) = first.run(arg)
    val SemaResult(r2, d2) = second.run(r1)
    SemaResult(r2, d1 :++ d2)
  }
}

private object NameCorrectnessCheckPass extends Pass[List[Decl], AST] {
  private def check(n: AstNode, names: HashSet[Name]): SemaResult[HashSet[Name]] =
    n match {
      case d @ FnDecl(name, params, rettype, body) =>
        val SemaResult(_, d) = check(body, names + name.value ++ params.map(_._1.value))
        SemaResult(names + name.value, d)
      case d @ VarDecl(const, name, tp, value) =>
        val SemaResult(_, d) = check(value, names)
        SemaResult(names + name.value, d)
      case BlockStmt(block) => {
        val SemaResult(_, d) = block.foldLeft(SemaResult(names))((res, stmt) => {
          val SemaResult(n, d) = check(stmt, res.value)
          SemaResult(n, res.diagnostics :++ d)
        })
        SemaResult(names, d)
      }
      case BoolLitExpr(_) | NumLitExpr(_, _) => SemaResult(names)
      case DeclStmt(decl)                    => check(decl, names)
      case IfStmt(cond, tBr, fBr) =>
        SemaResult(
          names,
          check(cond, names).diagnostics :++ check(tBr, names).diagnostics :++ fBr
            .map(check(_, names).diagnostics)
            .getOrElse(Nil),
        )
      case WhileStmt(cond, body) =>
        SemaResult(names, check(cond, names).diagnostics :++ check(body, names).diagnostics)
      case RetStmt(e)     => check(e, names)
      case UnitRetStmt(_) => SemaResult(names)
      case ExprStmt(e)    => check(e, names)
      case BinExpr(op, lhs, rhs) =>
        SemaResult(names, check(lhs, names).diagnostics :++ check(rhs, names).diagnostics)
      case UnaryExpr(op, e) => check(e, names)
      case VarRefExpr(name) =>
        SemaResult(
          names,
          if names.contains(name.value) then Nil
          else Diagnostic.error(name.span, s"reference to undefined name `${name.value}`") :: Nil,
        )
      case CallExpr(name, args) => {
        val nameCheck =
          if names.contains(name.value) then Nil
          else Diagnostic.error(name.span, s"reference to undefined name `${name.value}`") :: Nil
        SemaResult(
          names,
          args.foldLeft(nameCheck)((diags, e) => diags :++ check(e, names).diagnostics),
        )
      }
    }

  override def run(decls: List[Decl]): SemaResult[AST] = {
    var diagnostics = List[Diagnostic]()

    var names   = HashSet[Name]()
    var spanMap = HashMap[Name, Span]()

    for decl <- decls do {
      val name = decl.getName
      if names.contains(name.value) then {
        diagnostics :+= Diagnostic.error(
          Message(name.span, s"redefinition of `${name.value}`") ::
            Message(spanMap(name.value), s"previously defined here")
            :: Nil
        )
      } else {
        names += name.value
        spanMap += (name.value, name.span)
      }
    }

    for decl <- decls do {
      val SemaResult(_, diags) = check(decl, names)
      diagnostics :++= diags
    }

    SemaResult(
      if diagnostics.containsErrors
      then HashMap()
      else HashMap.from(decls.map(d => (d.getName.value, d))),
      diagnostics,
    )
  }
}

private object InitializerLoopCheckPass extends Pass[AST, AST] {
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
      case ExprStmt(e)          => getDeps(e, ctx)
      case DeclStmt(d)          => getDeps(d, ctx)
      case RetStmt(e)           => getDeps(e, ctx)
      case UnitRetStmt(_)       => (HashSet(), ctx)
      case BinExpr(_, lhs, rhs) => (getDeps(lhs, ctx)._1 ++ getDeps(rhs, ctx)._1, ctx)
      case UnaryExpr(_, e)      => getDeps(e, ctx)
      case VarRefExpr(name) =>
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

  override def run(ast: AST): SemaResult[AST] = {
    val deps: MapView[Name, HashSet[Name]] =
      ast.mapValues(getDeps(_, Ctx(ast, HashSet()))._1)

    var diagnostics = List[Diagnostic]()
    var newAst: AST = HashMap()
    var badNames    = HashSet[Name]()
    for (name, decl) <- ast do {
      decl match
        case d: FnDecl => newAst += (name, decl)
        case VarDecl(_, name, _, _) if deps(name.value).contains(name.value) =>
          diagnostics :+= Diagnostic.error(name.span, "dependency loop in initializer")
          badNames += name.value
        case VarDecl(_, _, _, _) => newAst += (name, decl)
    }

    SemaResult(
      newAst.filter(p => deps(p._1).intersect(badNames).isEmpty),
      diagnostics,
    )
  }
}

private object TypePropagationPass extends Pass[AST, AST] {
  private def propagateType(ifStmt: IfStmt, tp: Type, ast: AST): IfStmt = {
    val IfStmt(c, tBr, fBr) = ifStmt
    IfStmt(
      c,
      BlockStmt(propagateType(tBr.block, tp, ast)),
      fBr.map(s =>
        s match
          case b: BlockStmt => BlockStmt(propagateType(b.block, tp, ast))
          case ifs: IfStmt  => propagateType(ifs, tp, ast)
      ),
    )
  }

  private def propagateType(block: Block, tp: Type, ast: AST): Block = {
    block.map(
      _ match
        case b: BlockStmt       => BlockStmt(propagateType(b.block, tp, ast))
        case ExprStmt(e)        => ExprStmt(propagateTypes(e, ast))
        case s: IfStmt          => propagateType(s, tp, ast)
        case WhileStmt(c, body) => WhileStmt(c, BlockStmt(propagateType(body.block, tp, ast)))
        case DeclStmt(d)        => DeclStmt(propagateType(d, ast))
        case RetStmt(e)         => RetStmt(propagateType(e, tp, ast))
        case u: UnitRetStmt     => u
    )
  }

  private def propagateTypes(e: Expr, ast: AST): Expr =
    e match
      case b @ BinExpr(op, lhs, rhs) =>
        if !op.value.isCmp then BinExpr(op, propagateTypes(lhs, ast), propagateTypes(rhs, ast))
        else b
      case vre @ VarRefExpr(name) => vre
      case CallExpr(name, args) => {
        ast.get(name.value).get match
          case FnDecl(name, params, rettype, body) =>
            val newArgs = args
              .zip(params)
              .map(arg => {
                val (e, (_, t)) = arg
                propagateType(e, t.value, ast)
              })
            CallExpr(name, newArgs)
          // this should be unreachable for now because variables are not callable
          case _ => ???
      }
      case UnaryExpr(op, e) => UnaryExpr(op, propagateTypes(e, ast))
      case n: NumLitExpr    => n
      case b: BoolLitExpr   => b

  private def propagateType(e: Expr, tp: Type, ast: AST): Expr =
    e match
      case b @ BinExpr(op, lhs, rhs) =>
        if !op.value.isCmp then
          BinExpr(op, propagateType(lhs, tp, ast), propagateType(rhs, tp, ast))
        else b
      case vre @ VarRefExpr(name) => vre
      case CallExpr(name, args) => {
        ast.get(name.value).get match
          case FnDecl(_, params, rettype, body) =>
            val newArgs = args
              .zip(params)
              .map(arg => {
                val (e, (_, t)) = arg
                propagateType(e, t.value, ast)
              })
            CallExpr(name, newArgs)
          // this should be unreachable for now because variables are not callable
          case _ => ???
      }
      case UnaryExpr(op, e)         => UnaryExpr(op, propagateType(e, tp, ast))
      case n @ NumLitExpr(_, value) => if tp.isIntegerType then NumLitExpr(tp, value) else n
      case BoolLitExpr(b)           => BoolLitExpr(b)

  private def propagateType(fn: FnDecl, ast: AST): FnDecl = {
    val FnDecl(n, p, r, body) = fn
    FnDecl(n, p, r, BlockStmt(propagateType(body.block, r.value, ast)))
  }

  private def propagateType(vard: VarDecl, ast: AST): VarDecl = {
    val VarDecl(c, n, t, value) = vard
    t match
      case WithSpan(Type.Undef, s) => vard
      case ws @ WithSpan(t, s)     => VarDecl(c, n, ws, propagateType(value, t, ast))
  }

  override def run(ast: AST): SemaResult[AST] = {
    ast.mapValuesInPlace((_, d) =>
      d match
        case fnd: FnDecl   => propagateType(fnd, ast)
        case vard: VarDecl => propagateType(vard, ast)
    )
    SemaResult(ast)
  }
}

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

private object IntLitTypeDeductionPass extends Pass[AST, AST] {
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
      case BinExpr(op, lhs, rhs) =>
        for {
          l <- deduceIntTypes(lhs)
          r <- deduceIntTypes(rhs)
        } yield BinExpr(op, l, r)
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

  override def run(ast: AST): SemaResult[AST] =
    var diagnostics: List[Diagnostic] = Nil
    ast.mapValuesInPlace((name, decl) => {
      val SemaResult(d, diags) = deduceIntTypes(decl)
      diagnostics :++= diags
      d
    })
    SemaResult(ast, diagnostics)
}

private def coerceTypes(lhs: Type, rhs: Type): Type =
  (lhs, rhs) match
    case (x, y) if x == y => x
    case (x, Type.Undef)  => x
    case (Type.Undef, y)  => y
    case _                => Type.Invalid

private object TypeDeductionPass extends Pass[AST, AST] {
  private type Ctx = HashMap[Name, Type]

  private def deduceTypes(d: VarDecl, ctx: Ctx): SemaResult[VarDecl] =
    val VarDecl(c, n, tp, value) = d
    tp.value match
      case Type.Undef =>
        deduceTypes(value, ctx).map(res => VarDecl(c, n, WithSpan(res._2, res._1.getSpan), res._1))
      case _ => SemaResult(d)

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
      case BinExpr(op, lhs, rhs) =>
        for {
          l <- deduceTypes(lhs, ctx)
          r <- deduceTypes(rhs, ctx)
        } yield (
          BinExpr(op, l._1, r._1),
          if op.value.isCmp then Type.Bool else coerceTypes(l._2, r._2),
        )
      case vre @ VarRefExpr(name) => SemaResult((vre, ctx(name.value)))
      case CallExpr(name, args) =>
        var diagnostics: List[Diagnostic] = Nil
        val newArgs = args.map(e =>
          val SemaResult(e2, d) = deduceTypes(e, ctx)
          diagnostics :++= d
          e2._1
        )
        SemaResult((CallExpr(name, newArgs), ctx(name.value)), diagnostics)
      case UnaryExpr(op, e) => deduceTypes(e, ctx).map(res => (UnaryExpr(op, res._1), res._2))
      case n @ NumLitExpr(tp, ws @ WithSpan(value, span)) => SemaResult((n, tp))
      case b: BoolLitExpr                                 => SemaResult((b, Type.Bool))

  override def run(ast: AST): SemaResult[AST] =
    var diagnostics: List[Diagnostic] = Nil
    var ctx: Ctx                      = HashMap.from(ast.mapValues(_.getType.value))
    SemaResult(
      ast.mapValuesInPlace((n, d) => {
        val SemaResult(newDecl, diags) = deduceTypes(d, ctx)
        ctx(n) = newDecl.getType.value
        newDecl
      }),
      diagnostics,
    )
}

private object ArgsAmountCheckPass extends Pass[AST, AST] {
  private def check(e: Expr, ast: AST): List[Diagnostic] =
    e match
      case BinExpr(_, lhs, rhs) => check(lhs, ast) :++ check(rhs, ast)
      case VarRefExpr(_)        => Nil
      case UnaryExpr(_, e)      => check(e, ast)
      case NumLitExpr(_, _)     => Nil
      case BoolLitExpr(_)       => Nil
      case CallExpr(WithSpan(name, span), args) =>
        val fn: FnDecl = ast.get(name).get match
          case f: FnDecl => f
          case _         => ??? // this should be unreachable for now
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

  override def run(ast: AST): SemaResult[AST] = {
    val diagnostics: List[Diagnostic] =
      ast.foldLeft(Nil)((diags, decl) => diags :++ check(decl._2, ast))
    SemaResult(ast, diagnostics)
  }
}

private object TypeCheckPass extends Pass[AST, AST] {
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
          ctx ++ params.map(arg => arg._1.value -> (arg._2.value -> None)),
        )
      case VarDecl(const, name, tp, value) => typeCheck(value, Some(tp.value), ctx)._2

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
          val (t, d) = typeCheck(e, Some(tp.value), ctx)
          val d2 = if !d.isEmpty then {
            val last = d.last
            d.init :+ Diagnostic(
              last.severity,
              last.messages :+ Message(
                tp.span,
                "function return type defined here",
              ),
            )
          } else d
          ctx -> (diags :++ d2)
        case UnitRetStmt(s) =>
          val d =
            if tp.value == Type.Unit
            then Nil
            else
              Diagnostic.error(
                Message(s, s"expected ${tp.value}, but got ${Type.Unit}") ::
                  Message(tp.span, "function return type defined here") ::
                  Nil
              ) :: Nil
          ctx -> (diags :++ d)
    })._2

  private def typeCheck(e: Expr, tp: Option[Type], ctx: Ctx): (Type, List[Diagnostic]) =
    e match
      case BinExpr(op, lhs, rhs) =>
        val expectedType = if op.value.isCmp then None else tp
        val (t1, d1)     = typeCheck(lhs, expectedType, ctx)
        val (t2, d2)     = typeCheck(rhs, expectedType, ctx)
        var d            = d1 :++ d2
        val t = if !checkTypes(t1, t2) then {
          d :+= Diagnostic.error(
            op.span,
            s"unmatching types of operands: left is $t1, right is $t2",
          )
          Type.Invalid
        } else if t1 == Type.Invalid || t2 == Type.Invalid then Type.Invalid
        else t1
        val opCheckDiags =
          if tp.isDefined && op.value.isCmp && !checkTypes(tp.get, Type.Bool)
          then Diagnostic.error(op.span, s"expected ${tp.get}, but got ${Type.Bool}") :: Nil
          else Nil
        val opTp =
          if op.value.isCmp then (if opCheckDiags.isEmpty then Type.Bool else Type.Invalid)
          else t
        (opTp, d :++ opCheckDiags)
      case VarRefExpr(name) =>
        val v: VarDecl = ctx.get(name.value).get._2.get match
          case v: VarDecl => v
          case _          => ??? // this should not be reachable
        if tp.isEmpty || checkTypes(tp.get, v.tp.value)
        then (v.tp.value, Nil)
        else
          Type.Invalid -> (Diagnostic.error(
            name.span,
            s"expected ${tp.get}, got ${v.tp.value}",
          ) :: Nil)
      case CallExpr(name, args) =>
        val fn: FnDecl = ctx.get(name.value).get._2.get match
          case fn: FnDecl => fn
          case _          => ??? // this should not be reachable
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
        else (Type.Invalid, Diagnostic.error(value.span, s"expected ${tp.get}, but got $t") :: Nil)
      case BoolLitExpr(value) =>
        if tp.isEmpty || checkTypes(Type.Bool, tp.get) then (Type.Bool, Nil)
        else
          Type.Invalid -> (Diagnostic.error(
            value.span,
            s"expected ${tp.get}, but got ${Type.Bool}",
          ) :: Nil)

  override def run(ast: AST): SemaResult[AST] =
    var ctx: Ctx = HashMap.from(ast.mapValues(d => (d.getType.value, Some(d))))
    val diagnostics = ast
      .map(p => {
        val (n, d) = p
        typeCheck(d, ctx)
      })
      .fold(Nil)(_ :++ _)
    SemaResult(ast, diagnostics)
}

trait Sema:
  def run(decls: List[Decl]): SemaResult[AST]

class DefaultSema extends Sema:
  /* TODO:
    - const-correctness check
    - `is callable?` check
   */
  final private val passes: Pass[List[Decl], AST] =
    NameCorrectnessCheckPass
      >> InitializerLoopCheckPass
      >> ArgsAmountCheckPass
      >> TypePropagationPass
      >> IntLitTypeDeductionPass
      >> TypeDeductionPass
      >> TypeCheckPass

  def run(decls: List[Decl]): SemaResult[AST] = passes.run(decls)
