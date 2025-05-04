package backend.ir.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import backend.ir.irvalue.*

sealed trait Instr extends Value

sealed trait UnaryOp:
  def getArg: Value

sealed trait BinaryOp:
  def getArgs: (Value, Value)

sealed trait VarOp:
  def getArgs: List[Value]

sealed trait NonVoid:
  def getDest: Var

case class Add(dest: Var, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)
  def stMatch(v: Val): Boolean =
    v match
      case Val.Add(mdest, mlhs, mrhs) =>
        (dest `stMatch` mdest) && (lhs `stMatch` mlhs) && (rhs `stMatch` mrhs)
      case _ => false
  def getDest: Var            = dest
  def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType   = dest.vtype
  override def toString: String =
    s"$dest = add $vtype $lhs, $rhs;"

case class Sub(dest: Var, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def getDest: Var            = dest
  def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType   = dest.vtype
  override def toString: String =
    s"$dest = sub $vtype $lhs, $rhs;"

case class Neg(dest: Var, operand: Value) extends Value with Instr with UnaryOp with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def getDest: Var           = dest
  override def getArg: Value = operand
  override def vtype: VType  = dest.vtype
  override def toString: String =
    s"$dest = neg $vtype $operand"

case class Mul(dest: Var, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???

  def getDest: Var                     = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = mul $vtype $lhs, $rhs;"

case class Div(dest: Var, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def getDest: Var                     = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = dest.vtype
  override def toString: String =
    s"$dest = div $vtype $lhs, $rhs;"

case class Mov(lhs: Var, rhs: Value) extends Value with Instr with UnaryOp with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def getDest: Var           = lhs
  override def getArg: Value = rhs
  override def vtype: VType  = lhs.vtype
  override def toString: String =
    s"$lhs = mov $vtype $rhs;"

case class Call(dest: Var, fn: Label, args: List[Value])
    extends Value
    with Instr
    with VarOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def getDest: Var          = dest
  def getArgs: List[Value]  = args
  override def vtype: VType = dest.vtype
  override def toString: String =
    s"$dest = call $vtype $fn " + args.mkString(", ") + ";"

case class Jmp(label: Label) extends Value with Instr:
  def stMatch(v: Val): Boolean =
    ???
  override def vtype: VType = VType.Unit
  override def toString: String =
    s"jmp $label"

case class Br(cond: Value, tbranch: Label, fbranch: Label) extends Value with Instr with UnaryOp:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  override def getArg: Value = cond
  override def vtype: VType  = cond.vtype
  override def toString: String =
    s"br $vtype $cond, $tbranch ${tbranch.vtype}, $fbranch ${fbranch.vtype}"

case class Ret(ret: Value) extends Value with Instr with UnaryOp:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
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

case class Phi(dest: Var, var vals: List[Var], var defined: List[Label])
    extends Value
    with Instr
    with VarOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def rename(l: Label, stacks: HashMap[Var, Stack[Var]]): Phi =
    vals = vals.zip(defined).map((v, label) => if label == l then stacks(v).top else v)
    this

  def add(v: Var, l: Label): Unit =
    vals :+= v
    defined :+= l
  override def getDest: Var         = dest
  override def getArgs: List[Value] = vals
  override def vtype: VType         = dest.vtype
  override def toString: String = s"$dest = phi $vtype "
    + vals.zip(defined).mkString(", ")

case class Cmp(dest: Var, pred: Predicate, lhs: Value, rhs: Value)
    extends Value
    with Instr
    with BinaryOp
    with NonVoid:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  override def getDest: Var            = dest
  override def getArgs: (Value, Value) = (lhs, rhs)
  override def vtype: VType            = VType.Bool
  override def toString: String =
    s"$dest = cmp $vtype $pred $lhs $rhs"
