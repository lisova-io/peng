package backend.irgen.asttranslator

import frontend.ast.*
import frontend.lex.WithSpan
import backend.ir.ir.*
import backend.ir.control.*
import backend.ir.irvalue.*
import backend.irgen.irbuilder.BBuilder
import backend.irgen.irbuilder.FnBuilder
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.*
import scala.collection.mutable

// TODO: Generic wrap for logging

// type ASTType  = WithSpan[Type]
type ASTFnArg = (WithSpan[String], WithSpan[Type])

sealed trait ASTTranslator:
  def genNode(node: AstNode): Value
  def gen: Program

sealed trait TranslatorCtx:
  def genVirtualReg(vtype: VType, fn: FnBuilder, l: Label): Var
  def genLabel: Label
  def genIfThenLabel: Label
  def genIfElseLabel: Label
  def genLoopLabel: Label
  def genLoopCondLabel: Label
  def genLoopExitLabel: Label
  def genAfterIf: Label

sealed class DefaultCtx extends TranslatorCtx:
  var regCounter: Int         = 0
  var ifThenLabelCounter: Int = 0
  var ifElseLabelCounter: Int = 0
  var loopCondCounter: Int    = 0
  var loopLabelCounter: Int   = 0
  var loopExitLabel: Int      = 0
  var afterIf: Int            = 0

  def genAfterIf: Label =
    afterIf += 1
    Label("%after.if" + afterIf)

  def genLoopLabel: Label =
    loopLabelCounter += 1
    Label("%loop.body" + loopLabelCounter)

  def genLoopExitLabel: Label =
    loopExitLabel += 1
    Label("%loop.exit" + loopLabelCounter)

  def genLoopCondLabel: Label =
    loopCondCounter += 1
    Label("%loop.cond" + loopCondCounter.toString)

  def genIfThenLabel: Label =
    ifThenLabelCounter += 1
    Label("%if.then" + ifThenLabelCounter.toString)

  def genIfElseLabel: Label =
    ifElseLabelCounter += 1
    Label("%if.else" + ifElseLabelCounter.toString)

  def genVirtualReg(vtype: VType, fn: FnBuilder, l: Label): Var =
    regCounter += 1
    val v = Var("%" + regCounter.toString, vtype)
    v

  def genLabel: Label =
    regCounter += 1
    Label("%" + regCounter.toString)

sealed class LoggingCtx extends DefaultCtx with StrictLogging:
  override def genVirtualReg(vtype: VType, fn: FnBuilder, l: Label): Var =
    logger.debug(s"call of genVirtualReg $vtype, prev counter $regCounter")
    super.genVirtualReg(vtype, fn, l)
  override def genLabel: Label =
    logger.debug(s"call of genName, prev counter $regCounter")
    super.genLabel

sealed class DefaultTranslator(ast: AST, ctx: TranslatorCtx = DefaultCtx()) extends ASTTranslator:
  val blockBuilder: BBuilder         = BBuilder()
  val fnBuilder: FnBuilder           = FnBuilder()
  val fns: HashMap[String, Function] = HashMap()

  private def genPredicate(op: BinOp): Predicate =
    op match
      case BinOp.Eq => Predicate.eq
      case BinOp.Ge => Predicate.ge
      case BinOp.Gt => Predicate.gt
      case BinOp.Le => Predicate.le
      case BinOp.Lt => Predicate.lt
      case BinOp.Ne => Predicate.neq
      case _        => ???

  private def createJmp(label: Label, block: BasicBlock): Unit =
    val pred = block.name
    fnBuilder.addPred(label, pred)
    block.addInstruction(Jmp(label))

  private def createJmp(label: Label): Unit =
    val pred = blockBuilder.name
    fnBuilder.addPred(label, pred)
    blockBuilder.addInstr(Jmp(label))

  private def createBr(cond: Value, tbranch: Label, fbranch: Label): Unit =
    val pred = blockBuilder.name
    fnBuilder.addPred(tbranch, pred)
    fnBuilder.addPred(fbranch, pred)
    blockBuilder.addInstr(Br(cond, tbranch, fbranch))

  private def createVar(name: String, tp: VType): Var =
    val v = Var(name, tp)
    // fnBuilder.addVar(v, blockBuilder.name)
    v

  private def definedVar(name: String, tp: VType): Var =
    val v = Var(name, tp)
    fnBuilder.addVar(v, blockBuilder.name)
    v

  /*
   * Generate intermediate representation for binary expression.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  protected def genNodeBin(op: BinOp, lhs: Expr, rhs: Expr, tp: VType): Value =
    val left  = genNode(lhs)
    val right = genNode(rhs)
    val dest = op match
      case BinOp.Assign =>
        left match
          case v: Var =>
            fnBuilder.addVar(v, blockBuilder.name)
            v
          case _ => ???
      case _ => ctx.genVirtualReg(tp, fnBuilder, blockBuilder.name)
    op match
      case BinOp.Plus =>
        blockBuilder.addInstr(Add(dest, left, right))
      case BinOp.Minus =>
        blockBuilder.addInstr(Sub(dest, left, right))
      case BinOp.Mul =>
        blockBuilder.addInstr(Mul(dest, left, right))
      case BinOp.Assign =>
        blockBuilder.addInstr(Mov(dest, right))
      case BinOp.Lt | BinOp.Le | BinOp.Eq | BinOp.Ge | BinOp.Gt | BinOp.Ne =>
        blockBuilder.addInstr(Cmp(dest, genPredicate(op), left, right))
    dest

  /*
   * Internal function that transforms type from representation it is
   * in AST to the type that is used in IR
   */
  protected def astTypeToIR(tp: Type): VType =
    tp match
      case Type.I8                   => ???
      case Type.U8                   => ???
      case Type.I16                  => ???
      case Type.U16                  => ???
      case Type.I32                  => VType.I32
      case Type.U32                  => ???
      case Type.I64                  => ???
      case Type.U64                  => ???
      case Type.Bool                 => VType.Bool
      case Type.Unit                 => VType.Unit
      case Type.Undef | Type.Invalid => ??? // something went terribly wrong

  /*
   * Generate intermediate representation for function call expression.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  protected def genNodeCall(name: String, args: List[Expr]): Value =
    assert(ast.contains(name))
    val irArgs = args.map(genNode(_))
    ast(name) match
      case FnDecl(name, params, rtype, body) =>
        val fntype = astTypeToIR(rtype.value)
        val dest   = ctx.genVirtualReg(fntype, fnBuilder, blockBuilder.name)
        val label  = Label(name.value)
        val instr  = Call(dest, label, irArgs)
        blockBuilder.addInstr(instr)
        dest
      case _ => ???

  /*
   * Generate intermediate representation for variable declaration.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  protected def genNodeVDecl(
      const: Boolean,
      name: String,
      tp: Type,
      value: Expr,
  ): Value =
    val vtype = astTypeToIR(tp)
    val rhs   = genNode(value)
    // TODO: when typechecker is ready, fix or assert
    // val lhs = Var(name, rhs.vtype)
    val lhs = definedVar(name, rhs.vtype)
    blockBuilder.addInstr(Mov(lhs, rhs))
    lhs

  /*
   * Ends the current basic block, builds it,
   * adds it to the current function builder.
   */
  protected def blockEnd: BasicBlock =
    val block = blockBuilder.build
    fnBuilder.addBlock(block)
    blockBuilder.reset
    val label = ctx.genLabel
    blockBuilder.setName(label)
    block

  protected def blockStart(next: Label): BasicBlock =
    val block = blockBuilder.build
    fnBuilder.addBlock(block)
    blockBuilder.reset
    blockBuilder.setName(next)
    block

  /*
   * Ends the function, builds it, saves it in hashmap.
   */
  protected def fnEnd: Value =
    val fn = fnBuilder.build
    fns.addOne((fn.name.name, fn))
    fn

  protected def genNodeRet(): Value =
    val void = Void()
    blockBuilder.addInstr(Ret(void))
    blockEnd
    void

  protected def genNodeRet(expr: Expr): Value =
    val value = genNode(expr)
    blockBuilder.addInstr(Ret(value))
    blockEnd
    value

  protected def genBlock(block: Block): Value =
    block.foreach(genNode(_))
    Void()

  protected def genVarRef(name: String, tp: Type): Value =
    createVar(name, astTypeToIR(tp))

  protected def fullReset: Unit =
    fnBuilder.reset
    blockBuilder.reset

  protected def genWhile(cond: Expr, body: BlockStmt): Value =
    val condLabel = ctx.genLoopCondLabel
    val bodyLabel = ctx.genLoopLabel
    val endLabel  = ctx.genLoopExitLabel
    createJmp(condLabel)
    blockStart(condLabel)
    val condReg = genNode(cond)
    createBr(condReg, bodyLabel, endLabel)
    blockStart(bodyLabel)
    genNode(body)
    createJmp(condLabel)
    blockStart(endLabel)

  protected def genIf(
      cond: Expr,
      onTrue: BlockStmt,
      onFalse: Option[BlockStmt | IfStmt],
  ): Label =
    val condVal   = genNode(cond)
    val trueLabel = ctx.genIfThenLabel
    val outLabel: Label = onFalse match
      case None => ctx.genAfterIf
      case _    => ctx.genIfElseLabel
    createBr(condVal, trueLabel, outLabel)
    blockStart(trueLabel)
    genNode(onTrue)
    onFalse match
      case Some(IfStmt(cond, onTrue, onFalse)) =>
        val block       = blockStart(outLabel)
        val newOutLabel = genIf(cond, onTrue, onFalse)
        createJmp(newOutLabel, block)
        newOutLabel
      case None =>
        createJmp(outLabel)
        blockStart(outLabel)
        outLabel
      case Some(BlockStmt(block)) =>
        val actualOutLabel = ctx.genAfterIf
        createJmp(actualOutLabel)
        blockStart(outLabel)
        block.foreach(genNode(_))
        createJmp(actualOutLabel)
        blockStart(actualOutLabel)
        actualOutLabel

  // TODO: make it protected and log that shit
  private def genFunction(
      name: String,
      params: List[ASTFnArg],
      rtype: Type,
      body: Block,
  ) =
    // fullReset
    fnBuilder.reset
    fnBuilder.setName(Label(name)) // maybe have to do this label to. dk for now.
    blockBuilder.setName(Label(name))
    fnBuilder.setType(astTypeToIR(rtype))
    val paramsNoSpan = params.foreach((astArg, astType) => {
      val arg   = astArg.value
      val vtype = astTypeToIR(astType.value)
      fnBuilder.addArg(definedVar(arg, vtype))
    })
    val irBody = genBlock(body)
    fnEnd

  def genDecl(decl: Decl): Value =
    decl match
      case VarDecl(const, name, tp, value) => genNodeVDecl(const, name.value, tp.value, value)
      case FnDecl(name, params, rettype, body) =>
        genFunction(name.value, params, rettype.value, body.block)

  def genNode(node: AstNode): Value =
    node match
      case NumLitExpr(tp, value)           => ImmInt(value.value)
      case BinExpr(op, lhs, rhs, tp)       => genNodeBin(op.value, lhs, rhs, astTypeToIR(tp))
      case CallExpr(name, args)            => genNodeCall(name.value, args)
      case VarDecl(const, name, tp, value) => genNodeVDecl(const, name.value, tp.value, value)
      case RetStmt(expr)                   => genNodeRet(expr)
      case UnitRetStmt(_)                  => genNodeRet()
      case BlockStmt(block)                => genBlock(block)
      case VarRefExpr(name, tp)            => genVarRef(name.value, tp)
      case DeclStmt(decl)                  => genDecl(decl)
      case ExprStmt(expr)                  => genNode(expr)
      case FnDecl(name, params, rettype, body) =>
        genFunction(name.value, params, rettype.value, body.block)
      case WhileStmt(cond, body)         => genWhile(cond, body)
      case IfStmt(cond, onTrue, onFalse) => genIf(cond, onTrue, onFalse)
      case _ =>
        println(s"genNode развал ${node.getClass().getName()}")
        ???

  def gen: Program =
    ast.foreach((_, decl) => genNode(decl))
    Program(fns)

/*
 *
 * Logging wrapper on top of normal AST to IR translator.
 * Logs call of each function and arguments it's been called with.
 */
final class LoggingTranslator(ast: AST, ctx: TranslatorCtx = LoggingCtx())
    extends DefaultTranslator(ast, ctx)
    with StrictLogging:
  private def logCall[T](name: String, res: => T, args: Any*): T =
    val argString = args.mkString(", ")
    if argString.length != 0 then logger.debug(s"call to $name, args: $argString")
    else logger.debug(s"call to $name")
    res
  override protected def astTypeToIR(tp: Type): VType =
    logCall("astTypeToIR", super.astTypeToIR(tp), tp)

  override protected def blockEnd: BasicBlock =
    logCall("blockEnd", super.blockEnd)

  override protected def genNodeBin(op: BinOp, lhs: Expr, rhs: Expr, tp: VType): Value =
    logCall("genNodeBin", super.genNodeBin(op, lhs, rhs, tp), op, lhs, rhs, tp)

  override protected def fnEnd: Value =
    logCall("fnEnd", super.fnEnd)

  override protected def genBlock(block: Block): Value =
    logCall("genBlock", super.genBlock(block), block)

  override protected def genVarRef(name: String, tp: Type): Value =
    logCall("genVarRef", super.genVarRef(name, tp), name, tp)

  override protected def genNodeCall(name: String, args: List[Expr]): Value =
    logCall("genNodeCall", super.genNodeCall(name, args), name, args.mkString(", "))

  override protected def genNodeRet(): Value =
    logCall("genNodeRet(void)", super.genNodeRet())

  override def gen: Program =
    logCall("gen", super.gen)

  override protected def genNodeRet(expr: Expr): Value =
    logCall("genNodeRet", super.genNodeRet(expr), expr)

  override def genDecl(decl: Decl): Value =
    logCall("genDecl", super.genDecl(decl), decl)

  override def genNode(node: AstNode): Value =
    logCall("genNode", super.genNode(node), node)

  override protected def genNodeVDecl(
      const: Boolean,
      name: String,
      tp: Type,
      value: Expr,
  ): Value =
    logCall("genNodeVDecl", super.genNodeVDecl(const, name, tp, value), const, name, tp, value)

  override protected def fullReset: Unit =
    logCall("fullReset", super.fullReset)
