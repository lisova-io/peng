package backend.ir

import backend.irvalue._

abstract class Instr:
  def vtype: VType

case class Add(dest: Value, lhs: Value, rhs: Value) extends Instr:
  override def vtype: VType = dest.vtype
  override def toString(): String =
    s"$dest = $lhs + $rhs;"

case class Sub(dest: Value, lhs: Value, rhs: Value) extends Instr:
  override def vtype: VType = dest.vtype
  override def toString(): String =
    s"$dest = $lhs - $rhs;"

case class Mul(dest: Value, lhs: Value, rhs: Value) extends Instr:
  override def vtype: VType = dest.vtype
  override def toString(): String =
    s"$dest = $lhs * $rhs;"

case class Div(dest: Value, lhs: Value, rhs: Value) extends Instr:
  override def vtype: VType = dest.vtype
  override def toString(): String =
    s"$dest = $lhs / $rhs;"

case class Mov(lhs: Value, rhs: Value) extends Instr:
  override def toString(): String =
    s"$lhs = mov $rhs;"

case class Call(dest: Value, fn: Value, args: List[Value]) extends Instr:
  override def toString(): String =
    s"$dest = call $fn" + args.foldLeft((acc: String, arg: Value) => acc + " " + arg) + ";"

case class Jmp(label: Value) extends Instr:
  override def toString(): String =
    s"jmp $label"

case class Br(cond: Value, tbranch: Value, fbranch: Value) extends Instr:
  override def toString(): String =
    s"br $cond $tbranch $fbranch"

case class Ret(ret: Value) extends Instr:
  override def toString(): String =
    s"ret $ret"

enum Predicate:
  case eq
  case lt
  case gt

case class Cmp(dest: Value, pred: Predicate, lhs: Value, rhs: Value) extends Instr:
  override def toString(): String =
    s"$dest = cmp $pred $lhs $rhs"
