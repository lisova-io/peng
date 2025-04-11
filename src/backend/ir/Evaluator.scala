package backend.ir.evaluator

import backend.ir.control.*
import backend.ir.irvalue.*
import scala.collection.mutable.HashMap
import backend.ir.ir.*

trait EvalValue

class EInt(val i: BigInt) extends EvalValue:
  override def toString(): String =
    s"$i i32"
class EBool(val b: Boolean) extends EvalValue:
  override def toString(): String =
    s"$b boolean"
class EVoid extends EvalValue:
  override def toString(): String =
    s"void"

// TODO: make it into overseer entry
class Eval(p: Program):
  private var vregs: HashMap[Var, EvalValue]       = HashMap()
  private var blockMap: HashMap[Label, BasicBlock] = HashMap()
  private def getValue(v: Value): EvalValue =
    v match
      case v: Var         => vregs(v)
      case ImmBool(input) => EBool(input)
      case ImmInt(input)  => EInt(input)

  private def putValue(v: Var, ev: EvalValue): Unit =
    vregs.contains(v) match
      case true  => vregs(v) = ev
      case false => vregs.addOne((v, ev))

  def eval: EvalValue =
    p.fns.foreach((_, fn) =>
      fn.ssa
      fn.destroySSA
    )
    val mainFn = p.fns.get("main").get
    eval(mainFn)
  private def eval(fn: Function): EvalValue =
    val oldOne = blockMap
    blockMap = fn.blockMap
    val entry = blockMap(fn.name)
    val ret   = eval(entry)
    blockMap = oldOne
    println(s"fn ${fn.name} returned $ret")
    ret

  private def eval(instr: Instr): EvalValue =
    instr match
      case add: Add   => eval(add)
      case sub: Sub   => eval(sub)
      case div: Div   => eval(div)
      case mul: Mul   => eval(mul)
      case call: Call => eval(call)
      case jmp: Jmp   => eval(jmp)
      case br: Br     => eval(br)
      case ret: Ret   => eval(ret)
      case cmp: Cmp   => eval(cmp)
      case mov: Mov   => eval(mov)
      case _          => ???

  private def eval(block: BasicBlock): EvalValue =
    block.instrs.map(i => eval(i)).last

  private def eval(j: Jmp): EvalValue =
    eval(blockMap(j.label))

  private def eval(mov: Mov): EvalValue =
    val lhs = mov.lhs
    val rhs = mov.rhs
    val v   = getValue(rhs)
    putValue(lhs, v)
    v
  private def eval(add: Add): EvalValue =
    val lhs = getValue(add.lhs) match
      case i: EInt => i
      case _       => ???
    val rhs = getValue(add.rhs) match
      case i: EInt => i
      case _       => ???
    val ret = EInt(lhs.i + rhs.i)
    putValue(add.dest, ret)
    ret
  private def eval(sub: Sub): EvalValue =
    val lhs = getValue(sub.lhs) match
      case i: EInt => i
      case _       => ???
    val rhs = getValue(sub.rhs) match
      case i: EInt => i
      case _       => ???
    val ret = EInt(lhs.i - rhs.i)
    putValue(sub.dest, ret)
    ret
  private def eval(mul: Mul): EvalValue =
    val lhs = getValue(mul.lhs) match
      case i: EInt => i
      case _       => ???
    val rhs = getValue(mul.rhs) match
      case i: EInt => i
      case _       => ???
    val ret = EInt(lhs.i * rhs.i)
    putValue(mul.dest, ret)
    ret

  private def eval(div: Div): EvalValue =
    val lhs = getValue(div.lhs) match
      case i: EInt => i
      case _       => ???
    val rhs = getValue(div.rhs) match
      case i: EInt => i
      case _       => ???
    val ret = EInt(lhs.i / rhs.i)
    putValue(div.dest, ret)
    ret

  private def eval(call: Call): EvalValue =
    val newHm: HashMap[Var, EvalValue] = HashMap()
    val args                           = p.fns(call.fn.name).args.zip(call.args)
    for (param, arg) <- args
    do
      // yeah this pattern is all over the place
      // the only coping point why is that is that i dream of peace and of the
      // fact that someday that shit will actually be throwing correct exceptions
      // and diagnostics and stuff, and just asInstance will not be nearly as good
      val vparam = param match
        case v: Var => v
        case _      => ???
      newHm.addOne((vparam, getValue(arg)))
    val oldOne = vregs
    vregs = newHm
    val ret = eval(p.fns(call.fn.name))
    vregs = oldOne
    ret

  private def eval(br: Br): EvalValue =
    val cond = getValue(br.cond) match
      case b: EBool => b
      case _        => ???
    if cond.b then eval(blockMap(br.tbranch)) else eval(blockMap(br.fbranch))

  private def eval(ret: Ret): EvalValue =
    ret.vtype match
      case VType.Unit => EVoid()
      case _          => getValue(ret.ret)

  private def eval(cmp: Cmp): EvalValue =
    def predEvalInt(i1: EInt, i2: EInt): Boolean =
      cmp.pred match
        case Predicate.ge  => i1.i >= i2.i
        case Predicate.gt  => i1.i > i2.i
        case Predicate.le  => i1.i <= i2.i
        case Predicate.lt  => i1.i < i2.i
        case Predicate.eq  => i1.i == i2.i
        case Predicate.neq => i1.i != i2.i
    def predEvalBool(b1: EBool, b2: EBool): Boolean =
      cmp.pred match
        case Predicate.neq => b1 != b2
        case Predicate.eq  => b1 == b2
        case _             => ???
    val lhs = getValue(cmp.lhs)
    val rhs = getValue(cmp.rhs)
    val ret = lhs match
      case b1: EBool =>
        rhs match
          case b2: EBool => predEvalBool(b1, b2)
          case _         => ???
      case i1: EInt =>
        rhs match
          case i2: EInt => predEvalInt(i1, i2)
          case _        => ???
    val retEvaled = EBool(ret)
    putValue(cmp.dest, retEvaled)
    retEvaled
