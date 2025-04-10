package backend.ir.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import backend.ir.irvalue.*

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
    s"$dest = add $vtype $lhs, $rhs;"

case class Sub(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value          = dest
  def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType   = dest.vtype
  override def toString: String =
    s"$dest = sub $vtype $lhs, $rhs;"

case class Neg(dest: Value, operand: Value) extends Value with Instr with UnaryOp with NonVoid:

  def getDest: Value         = dest
  override def getArg: Value = operand
  override def vtype: VType  = dest.vtype
  override def toString: String =
    s"$dest = neg $vtype $operand"

case class Mul(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value                   = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = mul $vtype $lhs, $rhs;"

case class Div(dest: Value, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  def getDest: Value                   = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = div $vtype $lhs, $rhs;"

case class Mov(lhs: Value, rhs: Value) extends Value with Instr with UnaryOp with NonVoid:

  def getDest: Value         = lhs
  override def getArg: Value = rhs
  override def vtype: VType  = lhs.vtype
  override def toString: String =
    s"$lhs = mov $vtype $rhs;"

case class Call(dest: Value, fn: Label, args: List[Value])
    extends Value
    with Instr
    with VarOp
    with NonVoid:
  def getDest: Value        = dest
  def getArgs: List[Value]  = args
  override def vtype: VType = dest.vtype
  override def toString: String =
    s"$dest = call $vtype $fn " + args.mkString(", ") + ";"

case class Jmp(label: Label) extends Value with Instr:
  override def vtype: VType = VType.Unit
  override def toString: String =
    s"jmp $label"

case class Br(cond: Value, tbranch: Label, fbranch: Label) extends Value with Instr with UnaryOp:
  override def getArg: Value = cond
  override def vtype: VType  = cond.vtype
  override def toString: String =
    s"br $vtype $cond, $tbranch ${tbranch.vtype}, $fbranch ${fbranch.vtype}"

case class Ret(ret: Value) extends Value with Instr with UnaryOp:
  def getArg: Value         = ret
  override def vtype: VType = ret.vtype
  override def toString: String =
    s"ret $vtype $ret;"

enum Predicate:
  case eq
  case neq
  case lt
  case gt
  case le
  case ge

case class Phi(dest: Value, var vals: List[Var], var defined: List[Label])
    extends Value
    with Instr
    with VarOp
    with NonVoid:
  def rename(l: Label, stacks: HashMap[Var, Stack[Var]]): Phi =
    vals = vals.zip(defined).map((v, label) => if label == l then stacks(v).top else v)
    this

  def add(v: Var, l: Label): Unit =
    vals :+= v
    defined :+= l
  override def getDest: Value       = dest
  override def getArgs: List[Value] = vals
  override def vtype: VType         = dest.vtype
  override def toString: String = s"$dest = phi $vtype "
    + vals.zip(defined).mkString(", ")

case class Cmp(dest: Value, pred: Predicate, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:
  override def getDest: Value          = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = VType.Bool
  override def toString: String =
    s"$dest = cmp $vtype $pred $lhs $rhs"
