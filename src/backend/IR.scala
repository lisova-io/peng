package backend.ir

import backend.irvalue._

enum IType:
  case Add
  case Sub
  case Mul
  case Div
  case Mov
  case Call
  case Jmp
  case Ret
  case Br
  case Cmp

abstract class Instr():
  def itype: IType

case class Add(lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Add

case class Sub(lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Sub

case class Mul(lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Mul

case class Div(lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Div

case class Mov(lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Mov

case class Call(fn: Value, args: List[Value]) extends Instr:
  override def itype: IType = IType.Call

case class Jmp(label: Value) extends Instr:
  override def itype: IType = IType.Jmp

case class Br(cond: Value, tbranch: Value, fbranch: Value) extends Instr:
  override def itype: IType = IType.Br

case class Ret(ret: Value) extends Instr:
  override def itype: IType = IType.Ret

enum Predicate:
  case Equal
  case Lt
  case Gt

case class Cmp(pred: Predicate, lhs: Value, rhs: Value) extends Instr:
  override def itype: IType = IType.Cmp
