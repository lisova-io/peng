package backend.IR.IR

import ilist.*

trait Value:
  def getType: Type

enum Type:
  case UInt
  case Bool
  case Unit

class Program:
  val fns: List[Fn] = List()

class Fn(val name: String, val params: List[Var], val returnType: Type):
  val blocks: List[BasicBlock] = List()

case class BasicBlock(f: Fn) extends Value with IListNode[BasicBlock, Fn](None, None, f):
  def getType: Type = Type.Unit

abstract class Instr(b: BasicBlock) extends Value with IListNode[Instr, BasicBlock](None, None, b)

enum BinOp:
  case Add
  case Sub
  case Mul
  case Div

case class Bin(val dest: Var, val lhs: Value, val rhs: Value, val op: BinOp, b: BasicBlock)
    extends Instr(b):
  def getType: Type =
    assert(dest.getType == Type.UInt)
    dest.getType

enum Predicate:
  case LE
  case LT
  case GE
  case GT
  case EQ
  case NEQ

case class Cmp(val dest: Var, val lhs: Value, val rhs: Value, val p: Predicate, b: BasicBlock)
    extends Instr(b):
  def getType: Type =
    assert(dest.getType == Type.Bool)
    dest.getType

case class Jmp(val dest: BasicBlock, b: BasicBlock) extends Instr(b):
  def getType: Type = Type.Unit

case class Call(val dest: Var, val fn: Fn, val args: List[Value], b: BasicBlock) extends Instr(b):
  def getType: Type = dest.getType

case class Var(val name: String, tp: Type) extends Value:
  def getType: Type = tp

case class Br(
    val dest: Var,
    val cond: Var,
    val trueBranch: BasicBlock,
    val falseBranch: BasicBlock,
    b: BasicBlock,
) extends Instr(b):
  def getType: Type =
    assert(dest.getType == Type.Bool)
    dest.getType

case class ConstInt(val value: Int) extends Value:
  def getType: Type = Type.UInt

case class ConstBool(val value: Boolean) extends Value:
  def getType: Type = Type.Bool
