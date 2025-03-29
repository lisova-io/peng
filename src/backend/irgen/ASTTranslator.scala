package backend.irgen.asttranslator

import frontend.ast._
import frontend.lex.WithSpan
import backend.ir.ir._
import backend.ir.control._
import backend.ir.irvalue._
import backend.irgen.irbuilder.BBuilder
import backend.irgen.irbuilder.FnBuilder
import scala.collection.mutable.HashMap

type AST      = HashMap[Name, Decl]
type ASTType  = Option[WithSpan[String]]
type ASTFnArg = (WithSpan[String], WithSpan[String])

trait ASTTranslator:
  def genNode(node: AstNode): Value

trait TranslatorCtx:
  def genVirtualReg(vtype: VType): Var

class DefaultCtx extends TranslatorCtx:
  var counter: Int = 0

  def genVirtualReg(vtype: VType): Var =
    counter += 1
    Var("%" + counter.toString, vtype)

class DefaultTranslator(ast: AST, ctx: TranslatorCtx = DefaultCtx()) extends ASTTranslator:
  val blockBuilder: BBuilder         = BBuilder()
  val fnBuilder: FnBuilder           = FnBuilder()
  val fns: HashMap[String, Function] = HashMap()

  /*
   * Generate intermediate representation for binary expression.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  private def genNodeBin(op: BinOp, lhs: Expr, rhs: Expr): Value =
    val left  = genNode(lhs)
    val right = genNode(rhs)
    val vreg  = ctx.genVirtualReg(VType.i32)
    op match
      case BinOp.Plus =>
        blockBuilder.addInstr(Add(vreg, left, right))
      case BinOp.Minus =>
        blockBuilder.addInstr(Sub(vreg, left, right))
      case BinOp.Mul =>
        blockBuilder.addInstr(Mul(vreg, left, right))
    vreg

  /*
   * Internal function that transforms type from representation it is
   * in AST to the type that is used in IR
   */
  private def astTypeToIR(tp: ASTType): VType =
    def getType(str: String): VType =
      str match
        case "int"  => VType.i32
        case "bool" => VType.bool
    tp.map(span => getType(span.value)).getOrElse(VType.unit)

  /*
   * Generate intermediate representation for function call expression.
   * Side effect is that it adds instruction to the current block in the builder.
   * Returns name of the virtual register that is result of this expression.
   */
  private def genNodeCall(name: String, args: List[Expr]): Value =
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
  private def genNodeVDecl(
      const: Boolean,
      name: String,
      tp: ASTType,
      value: Expr
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
  private def blockEnd: Value =
    val block = blockBuilder.build
    fnBuilder.addBlock(block)
    block

  /*
   * Ends the function, builds it, saves it in hashmap.
   */
  private def fnEnd: Value =
    val fn = fnBuilder.build
    fns.addOne((fn.name, fn))
    fn

  private def genNodeRet(): Value =
    val void = Void()
    blockBuilder.addInstr(Ret(void))
    blockEnd
    void

  private def genNodeRet(expr: Expr): Value =
    val value = genNode(expr)
    blockBuilder.addInstr(Ret(value))
    blockEnd
    value

  private def genBlock(block: Block): Value =
    block.foreach(genNode(_))
    Void()

  private def genVarRef(name: String): Value =
    Var(name, VType.i32)

  private def fullReset: Unit =
    fnBuilder.reset
    blockBuilder.reset

  private def genFunction(
      name: String,
      params: List[ASTFnArg],
      rtype: ASTType,
      body: Block
  ) =
    // fullReset
    fnBuilder.reset
    fnBuilder.setName(name)
    blockBuilder.setName(name)
    val vtype = astTypeToIR(rtype)
    val paramsNoSpan = params.foreach((astArg, astType) => {
      val arg   = astArg.value
      val vtype = astTypeToIR(Some(astType))
      fnBuilder.addArg(Var(arg, vtype))
    })
    val irBody = body.foreach(genNode(_))
    fnEnd

  def genDecl(decl: Decl): Value =
    decl match
      case VarDecl(const, name, tp, value)     => genNodeVDecl(const, name.value, tp, value)
      case FnDecl(name, params, rettype, body) => genFunction(name.value, params, rettype, body)

  def genNode(node: AstNode): Value =
    node match
      case NumLitExpr(tp, value)               => ImmInt(value.value)
      case BinExpr(op, lhs, rhs)               => genNodeBin(op.value, lhs, rhs)
      case CallExpr(name, args)                => genNodeCall(name.value, args)
      case VarDecl(const, name, tp, value)     => genNodeVDecl(const, name.value, tp, value)
      case RetStmt(expr)                       => genNodeRet(expr)
      case VoidRetStmt                         => genNodeRet()
      case BlockStmt(block)                    => genBlock(block)
      case VarRefExpr(name)                    => genVarRef(name.value)
      case DeclStmt(decl)                      => genDecl(decl)
      case FnDecl(name, params, rettype, body) => genFunction(name.value, params, rettype, body)
      case _ =>
        println(s"genNode развал ${node.getClass().getName()}")
        ???

  def gen: HashMap[String, Function] =
    ast.foreach((_, decl) => genNode(decl))
    fns
