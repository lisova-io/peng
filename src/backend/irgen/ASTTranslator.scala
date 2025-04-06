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

type ASTType  = WithSpan[Type]
type ASTFnArg = (WithSpan[String], WithSpan[Type])

sealed trait ASTTranslator:
  def genNode(node: AstNode): Value
  def gen: HashMap[String, Function]

sealed trait TranslatorCtx:
  def genVirtualReg(vtype: VType): Var
  def genName: String

sealed class DefaultCtx extends TranslatorCtx:
  var counter: Int = 0

  def genVirtualReg(vtype: VType): Var =
    counter += 1
    Var("%" + counter.toString, vtype)

  def genName: String =
    counter += 1
    counter.toString

sealed class LoggingCtx extends DefaultCtx with StrictLogging:
  override def genVirtualReg(vtype: VType): Var =
    logger.debug(s"call of genVirtualReg $vtype, prev counter $counter")
    super.genVirtualReg(vtype)
  override def genName: String =
    logger.debug(s"call of genName, prev counter $counter")
    super.genName

sealed class DefaultTranslator(ast: AST, ctx: TranslatorCtx = DefaultCtx()) extends ASTTranslator:
  val blockBuilder: BBuilder         = BBuilder()
  val fnBuilder: FnBuilder           = FnBuilder()
  val fns: HashMap[String, Function] = HashMap()

  /*
   * Generate intermediate representation for binary expression.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  protected def genNodeBin(op: BinOp, lhs: Expr, rhs: Expr): Value =
    val left  = genNode(lhs)
    val right = genNode(rhs)
    val dest = op match
      case BinOp.Assign => left
      case _            => ctx.genVirtualReg(VType.I32)
    op match
      case BinOp.Plus =>
        blockBuilder.addInstr(Add(dest, left, right))
      case BinOp.Minus =>
        blockBuilder.addInstr(Sub(dest, left, right))
      case BinOp.Mul =>
        blockBuilder.addInstr(Mul(dest, left, right))
      case BinOp.Assign =>
        blockBuilder.addInstr(Mov(left, right))
      case BinOp.Eq => ???
      case BinOp.Ne => ???
      case BinOp.Lt => ???
      case BinOp.Le => ???
      case BinOp.Gt => ???
      case BinOp.Ge => ???
    dest

  /*
   * Internal function that transforms type from representation it is
   * in AST to the type that is used in IR
   */
  protected def astTypeToIR(tp: ASTType): VType =
    def getType(t: Type): VType =
      t match
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
    getType(tp.value)

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
        val fntype = astTypeToIR(rtype)
        val dest   = ctx.genVirtualReg(fntype)
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
      tp: ASTType,
      value: Expr,
  ): Value =
    val vtype = astTypeToIR(tp)
    val rhs   = genNode(value)
    // TODO: when typechecker is ready, fix or assert
    val lhs = Var(name, rhs.vtype)
    blockBuilder.addInstr(Mov(lhs, rhs))
    lhs

  /*
   * Ends the current basic block, builds it,
   * adds it to the current function builder.
   */
  protected def blockEnd: Value =
    val block = blockBuilder.build
    fnBuilder.addBlock(block)
    blockBuilder.reset
    blockBuilder.setName(ctx.genName)
    block

  /*
   * Ends the function, builds it, saves it in hashmap.
   */
  protected def fnEnd: Value =
    val fn = fnBuilder.build
    fns.addOne((fn.name, fn))
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

  protected def genVarRef(name: String): Value =
    Var(name, VType.I32)

  protected def fullReset: Unit =
    fnBuilder.reset
    blockBuilder.reset

  private def genFunction(
      name: String,
      params: List[ASTFnArg],
      rtype: ASTType,
      body: Block,
  ) =
    // fullReset
    fnBuilder.reset
    fnBuilder.setName(name)
    blockBuilder.setName(name)
    val vtype = astTypeToIR(rtype)
    val paramsNoSpan = params.foreach((astArg, astType) => {
      val arg   = astArg.value
      val vtype = astTypeToIR(astType)
      fnBuilder.addArg(Var(arg, vtype))
    })
    val irBody = genBlock(body)
    fnEnd

  def genDecl(decl: Decl): Value =
    decl match
      case VarDecl(const, name, tp, value) => genNodeVDecl(const, name.value, tp, value)
      case FnDecl(name, params, rettype, body) =>
        genFunction(name.value, params, rettype, body.block)

  def genNode(node: AstNode): Value =
    node match
      case NumLitExpr(tp, value)           => ImmInt(value.value)
      case BinExpr(op, lhs, rhs)           => genNodeBin(op.value, lhs, rhs)
      case CallExpr(name, args)            => genNodeCall(name.value, args)
      case VarDecl(const, name, tp, value) => genNodeVDecl(const, name.value, tp, value)
      case RetStmt(expr)                   => genNodeRet(expr)
      case UnitRetStmt(_)                  => genNodeRet()
      case BlockStmt(block)                => genBlock(block)
      case VarRefExpr(name)                => genVarRef(name.value)
      case DeclStmt(decl)                  => genDecl(decl)
      case ExprStmt(expr)                  => genNode(expr)
      case FnDecl(name, params, rettype, body) =>
        genFunction(name.value, params, rettype, body.block)
      case _ =>
        println(s"genNode развал ${node.getClass().getName()}")
        ???

  def gen: HashMap[String, Function] =
    ast.foreach((_, decl) => genNode(decl))
    fns

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
  override protected def astTypeToIR(tp: ASTType): VType =
    logCall("astTypeToIR", super.astTypeToIR(tp), tp)

  override protected def blockEnd: Value =
    logCall("blockEnd", super.blockEnd)

  override protected def genNodeBin(op: BinOp, lhs: Expr, rhs: Expr): Value =
    logCall("genNodeBin", super.genNodeBin(op, lhs, rhs), op, lhs, rhs)

  override protected def fnEnd: Value =
    logCall("fnEnd", super.fnEnd)

  override protected def genBlock(block: Block): Value =
    logCall("genBlock", super.genBlock(block), block)

  override protected def genVarRef(name: String): Value =
    logCall("genVarRef", super.genVarRef(name), name)

  override protected def genNodeCall(name: String, args: List[Expr]): Value =
    logCall("genNodeCall", super.genNodeCall(name, args), name, args.mkString(", "))

  override protected def genNodeRet(): Value =
    logCall("genNodeRet(void)", super.genNodeRet())

  override def gen: HashMap[String, Function] =
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
      tp: ASTType,
      value: Expr,
  ): Value =
    logCall("genNodeVDecl", super.genNodeVDecl(const, name, tp, value), const, name, tp, value)

  override protected def fullReset: Unit =
    logCall("fullReset", super.fullReset)
