package codegen.mir.mirgen

import backend.ir.control.Program
import backend.ir.irvalue._
import backend.ir.control._
import backend.ir.ir._
import codegen.mir.mvalue._
import codegen.mir.minstr._
import codegen.mir.mcontrol._

trait MIRGen:
  def gen(value: Value): MValue

class DefaultMIRGen(program: Program) extends MIRGen:

  def genAdd(dest: Value, lhs: Value, rhs: Value): MValue =
    MAdd(gen(dest), gen(lhs), gen(rhs))

  def genSub(dest: Value, lhs: Value, rhs: Value): MValue =
    MSub(gen(dest), gen(lhs), gen(rhs))

  def genMul(dest: Value, lhs: Value, rhs: Value): MValue =
    MMul(gen(dest), gen(lhs), gen(rhs))

  def genDiv(dest: Value, lhs: Value, rhs: Value): MValue =
    MDiv(gen(dest), gen(lhs), gen(rhs))

  def genCmp(dest: Value, pred: Predicate, lhs: Value, rhs: Value): MValue =
    MCmp(gen(dest), pred, gen(lhs), gen(rhs))

  def genLabel(label: Label): MLabel =
    MLabel(label.name)

  def genLabel(label: String): MLabel =
    MLabel(label)

  def genBr(cond: Value, tbranch: Label, fbranch: Label): MValue =
    MBr(gen(cond), genLabel(tbranch), genLabel(fbranch))

  def gen(value: Value): MValue =
    value match
      case Add(dest, lhs, rhs)        => genAdd(dest, lhs, rhs)
      case Sub(dest, lhs, rhs)        => genSub(dest, lhs, rhs)
      case Mul(dest, lhs, rhs)        => genMul(dest, lhs, rhs)
      case Div(dest, lhs, rhs)        => genDiv(dest, lhs, rhs)
      case Cmp(dest, pred, lhs, rhs)  => genCmp(dest, pred, lhs, rhs)
      case Br(cond, tbranch, fbranch) => genBr(cond, tbranch, fbranch)
      case Ret(ret)                   => MRet(gen(ret))
      case Jmp(label)                 => MJmp(genLabel(label))
      case Call(dest, fn, args)       => MCall(gen(dest), genLabel(fn), args.map(gen(_)))
      case Mov(lhs, rhs)              => MMov(gen(lhs), gen(rhs))
      case ImmInt(input)              => MInt(input)
      case ImmBool(input)             => MBool(input)
      case Var(input, tp)             => MVar(input, tp)
      case Label(name: String)        => genLabel(name)
      case _ =>
        println("развал в MIRGen'e " + value)
        ???
