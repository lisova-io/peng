package backend.ir

import backend.irvalue._

sealed abstract class Instr()

sealed case class Add(lhs: Value, rhs: Value) extends Instr

sealed case class Sub(lhs: Value, rhs: Value) extends Instr

sealed case class Mul(lhs: Value, rhs: Value) extends Instr

sealed case class Div(lhs: Value, rhs: Value) extends Instr

sealed case class Mov(lhs: Value, rhs: Value) extends Instr

sealed case class Call(fn: Value, args: List[Value]) extends Instr

sealed case class Jmp(label: Value) extends Instr

sealed case class Br(cond: Value, tbranch: Value, fbranch: Value) extends Instr

sealed case class Ret(ret: Value) extends Instr

enum Predicate:
  case Equal
  case Lt
  case Gt

sealed case class Cmp(pred: Predicate, lhs: Value, rhs: Value) extends Instr
