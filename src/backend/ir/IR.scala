package backend.ir.ir

import backend.ir.irvalue._

sealed trait Instr

sealed trait UnaryOp:
  def getArg: Value

sealed trait BinaryOp:
  def getArgs: (Value, Value)

sealed trait VarOp:
  def getArgs: List[Value]

sealed trait NonVoid:
  def getDest: Value

case class Add(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value          = dest
  def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType   = dest.vtype
  override def toString: String =
    s"$dest = add $lhs, $rhs;"

case class Sub(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value          = dest
  def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType   = dest.vtype
  override def toString: String =
    s"$dest = sub $lhs, $rhs;"

case class Neg(dest: Value, operand: Value) extends Value with Instr with UnaryOp with NonVoid:

  def getDest: Value         = dest
  override def getArg: Value = operand
  override def vtype: VType  = dest.vtype
  override def toString: String =
    s"$dest = neg $operand"

case class Mul(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value                   = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = mul $lhs, $rhs;"

case class Div(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value                   = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = div $lhs, $rhs;"

case class Mov(lhs: Value, rhs: Value) extends Value with Instr with UnaryOp with NonVoid:

  def getDest: Value         = lhs
  override def getArg: Value = rhs
  override def vtype: VType  = lhs.vtype
  override def toString: String =
    s"$lhs = mov $rhs;"

case class Call(dest: Value, fn: Value, args: List[Value]) extends Value with Instr with VarOp:
  def getArgs: List[Value]  = args
  override def vtype: VType = dest.vtype
  override def toString: String =
    s"$dest = call $fn" + args.foldLeft((acc: String, arg: Value) => acc + " " + arg) + ";"

case class Jmp(label: Value) extends Value with Instr:
  override def vtype: VType = VType.unit
  override def toString: String =
    s"jmp $label"

case class Br(cond: Value, tbranch: Value, fbranch: Value) extends Value with Instr with UnaryOp:
  override def getArg: Value = cond
  override def vtype: VType  = VType.unit
  override def toString: String =
    s"br $cond $tbranch $fbranch"

case class Ret(ret: Value) extends Value with Instr:
  override def vtype: VType = VType.unit
  override def toString: String =
    s"ret $ret;"

enum Predicate:
  case eq
  case lt
  case gt

case class Cmp(dest: Value, pred: Predicate, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:
  override def getDest: Value          = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = VType.bool
  override def toString: String =
    s"$dest = cmp $pred $lhs $rhs"
