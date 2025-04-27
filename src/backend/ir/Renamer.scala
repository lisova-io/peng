package backend.ir.renamer

import backend.ir.ir.*
import backend.ir.irvalue.*
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class Renamer:
  var counter: Int = 0
  def rrVar(v: Value, stacks: HashMap[Var, Stack[Var]]): Value =
    v match
      case v: Var =>
        if stacks.contains(v) then stacks(v).top
        else v
      case _ => v
  def rrVar(v: Var, stacks: HashMap[Var, Stack[Var]]): Var =
    if stacks contains v then stacks(v).top else v

  def rrPhi(phi: Phi, stacks: HashMap[Var, Stack[Var]], label: Label): Phi =
    val dest: Var = phi.dest
    stacks(dest).push(Var(s"%$counter", dest.vtype))
    val top = stacks(dest).top
    counter += 1
    val vals = phi.vals
      .zip(phi.defined)
      .map((oldV, l) => if l == label && dest == oldV then top else oldV)
    Phi(top, vals, phi.defined)

  def renameNV(instr: NonVoid, stacks: HashMap[Var, Stack[Var]], label: Label): Instr =
    val dest: Var = instr.getDest
    stacks(dest).push(Var(s"%$counter", dest.vtype))
    counter += 1
    instr match
      case Add(dest, lhs, rhs)       => Add(rrVar(dest, stacks), lhs, rhs)
      case Cmp(dest, pred, lhs, rhs) => Cmp(rrVar(dest, stacks), pred, lhs, rhs)
      case Div(dest, lhs, rhs)       => Div(rrVar(dest, stacks), lhs, rhs)
      case Mov(lhs, rhs)             => Mov(rrVar(lhs, stacks), rhs)
      case Mul(dest, lhs, rhs)       => Mul(rrVar(dest, stacks), lhs, rhs)
      case Neg(dest, operand)        => Neg(rrVar(dest, stacks), operand)
      case p: Phi                    => rrPhi(p, stacks, label)
      case Call(dest, fn, args)      => Call(rrVar(dest, stacks), fn, args)
      case Sub(dest, lhs, rhs)       => Sub(rrVar(dest, stacks), lhs, rhs)
  def renameInstr(instr: Instr, stacks: HashMap[Var, Stack[Var]]): Instr =
    instr match
      case Add(dest, lhs, rhs)       => Add(dest, rrVar(lhs, stacks), rrVar(rhs, stacks))
      case Cmp(dest, pred, lhs, rhs) => Cmp(dest, pred, rrVar(lhs, stacks), rrVar(rhs, stacks))
      case Div(dest, lhs, rhs)       => Div(dest, rrVar(lhs, stacks), rrVar(rhs, stacks))
      case Mov(lhs, rhs)             => Mov(lhs, rrVar(rhs, stacks))
      case Mul(dest, lhs, rhs)       => Mul(dest, rrVar(lhs, stacks), rrVar(rhs, stacks))
      case Neg(dest, operand)        => Neg(dest, rrVar(operand, stacks))
      case Phi(dest, vals, defined)  => Phi(dest, vals, defined)
      // case Phi(dest, vals, defined)   => Phi(dest, vals.map(v => rrVar(v, stacks)), defined)
      case Call(dest, fn, args)       => Call(dest, fn, args.map(arg => rrVar(arg, stacks)))
      case Sub(dest, lhs, rhs)        => Sub(dest, rrVar(lhs, stacks), rrVar(rhs, stacks))
      case Br(cond, tbranch, fbranch) => Br(rrVar(cond, stacks), tbranch, fbranch)
      case Jmp(label)                 => instr
      case Ret(ret)                   => Ret(rrVar(ret, stacks))
  def rename(instr: Instr, stacks: HashMap[Var, Stack[Var]], label: Label): Instr =
    instr match
      case nv: NonVoid =>
        renameInstr(instr, stacks) match
          case newnv: NonVoid => renameNV(newnv, stacks, label)
          case _              => ???
      case instruction => renameInstr(instruction, stacks)
