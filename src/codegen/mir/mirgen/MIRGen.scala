package codegen.mir.mirgen

import backend.ir.irvalue.*
import backend.ir.control.*
import backend.ir.ir.*
import codegen.mir.mvalue.*
import codegen.mir.minstr.*
import codegen.mir.mcontrol.*
import codegen.mir.mirgen.mirbuilder.*
import codegen.mir.register.AMD64Register
import codegen.mir.register.amdReg
import scala.collection.mutable.ArrayBuffer

trait MIRGen extends ValueVisitor[MValue]

// maybe it should return list instead of singular instruction.
// for now it is okay like that i think
trait ABI:
  def addReturn(tp: VType, mv: MValue): Vector[MInstr]
  def addArg(tp: VType, num: Int, mv: MValue): Vector[MInstr]
  def getParam(tp: VType, num: Int, mv: MValue): Vector[MInstr]
  def getReturned(tp: VType, mv: MValue): Vector[MInstr]

class SystemVABI extends ABI:
  val abiParams: ArrayBuffer[amdReg] =
    ArrayBuffer(amdReg.rdi, amdReg.rsi, amdReg.rdx, amdReg.rcx, amdReg.r8, amdReg.r9)

  def getReturned(tp: VType, mv: MValue): Vector[MInstr] =
    val mov = MMov(mv, AMD64Register(amdReg.rax))
    Vector(mov)

  def param(num: Int): amdReg =
    if num > abiParams.length then ???
    abiParams(num)

  def getParam(tp: VType, num: Int, mv: MValue): Vector[MInstr] =
    val reg = param(num)
    val mov = MMov(mv, AMD64Register(reg))
    Vector(mov)

  def addReturn(tp: VType, mv: MValue): Vector[MInstr] =
    val mov = MMov(AMD64Register(amdReg.rax), mv)
    Vector(mov)
  def addArg(tp: VType, num: Int, mv: MValue): Vector[MInstr] =
    val reg = param(num)
    val mov = MMov(AMD64Register(reg), mv)
    Vector(mov)

// flawed version. matcher is better, and we can do better than matcher
// with selection dags and sheise.
// this will have to do due to time constraints.
// need to dawg this shit up
class DefaultMIRGen(abi: ABI) extends MIRGen:
  val bBuilder: MBlockBuilder = MBlockBuilder()
  val fnBuilder: MFnBuilder   = MFnBuilder()

  private def endBlock: MBBlock =
    val block = bBuilder.build
    fnBuilder.add(block)
    bBuilder.reset
    block

  def visit(value: Value): MValue =
    value match
      case add: Add          => visit(add)
      case sub: Sub          => visit(sub)
      case mul: Mul          => visit(mul)
      case div: Div          => visit(div)
      case br: Br            => visit(br)
      case call: Call        => visit(call)
      case block: BasicBlock => visit(block)
      case fn: Function      => visit(fn)
      case mov: Mov          => visit(mov)
      case bool: ImmBool     => visit(bool)
      case int: ImmInt       => visit(int)
      case jmp: Jmp          => visit(jmp)
      case label: Label      => visit(label)
      case cmp: Cmp          => visit(cmp)
      case variable: Var     => visit(variable)
      case ret: Ret          => visit(ret)

  def visit(program: Program): MProgram =
// class Module(fns: HashMap[String, MFunction]):
    val fns = program.fns.map((s, fn) => s -> visit(fn))
    MProgram(fns)

  def visit(block: BasicBlock): MBBlock =
    bBuilder.set(visit(block.name))
    block.instrs.foreach(f => visit(f))
    endBlock

  // need to add getting param too
  def visit(fn: Function): MFunction =
    fnBuilder.set(fn.vtype)
    fnBuilder.set(visit(fn.name))

    val argReceivers =
      for (param, num) <- fn.args.zipWithIndex
      yield abi.getParam(param.vtype, num, visit(param))

    argReceivers.foreach(bBuilder.add(_))

    fn.blocks.foreach(b => visit(b))
    val mfn = fnBuilder.build
    fnBuilder.reset
    mfn

  def visit(int: ImmInt): MInt = MInt(int.input)

  def visit(bool: ImmBool): MBool = MBool(bool.input)

  def visit(label: Label): MLabel = MLabel(label.name)

  def visit(irvar: Var): MVar = MVar(irvar.input, irvar.vtype)

  def visit(add: Add): MAdd =
    val madd = MAdd(visit(add.dest), visit(add.lhs), visit(add.rhs))
    bBuilder.add(madd)
    madd

  def visit(sub: Sub): MSub =
    val msub = MSub(visit(sub.dest), visit(sub.lhs), visit(sub.rhs))
    bBuilder.add(msub)
    msub

  def visit(mul: Mul): MMul =
    val mmul = MMul(visit(mul.dest), visit(mul.lhs), visit(mul.rhs))
    bBuilder.add(mmul)
    mmul

  def visit(div: Div): MDiv =
    val mdiv = MDiv(visit(div.dest), visit(div.lhs), visit(div.rhs))
    bBuilder.add(mdiv)
    mdiv

  def visit(ret: Ret): MRet =
    ret.vtype match
      case VType.Unit => ()
      case _ =>
        val retVec = abi.addReturn(ret.vtype, visit(ret.ret))
        bBuilder.add(retVec)
    val mret = MRet()
    bBuilder.add(mret)
    mret

  def visit(mov: Mov): MMov =
    val mmov = MMov(visit(mov.lhs), visit(mov.rhs))
    bBuilder.add(mmov)
    mmov

  def visit(call: Call): MCall =
    val args = call.args
    args.foldLeft(0)((counter, v) =>
      val mov = abi.addArg(v.vtype, counter, visit(v))
      bBuilder.add(mov)
      counter + 1
    )
    val mcall = MCall(visit(call.fn))
    bBuilder.add(mcall)
    val mov = abi.getReturned(call.vtype, visit(call.dest))
    bBuilder.add(mov)
    mcall

  def visit(cmp: Cmp): MCmp =
    val mcmp = MCmp(cmp.pred, visit(cmp.lhs), visit(cmp.rhs))
    bBuilder.add(mcmp)
    mcmp

  def visit(jmp: Jmp): MJmp =
    val mjmp = MJmp(visit(jmp.label))
    bBuilder.add(mjmp)
    mjmp

  def visit(br: Br): MBr =
    val cmp = bBuilder.last match
      case m: MCmp => m
      case _       => ???
    val mbr = MBr(cmp.pred, visit(br.tbranch), visit(br.fbranch))
    bBuilder.add(mbr)
    mbr
