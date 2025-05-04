package codegen.mir.minstr

import codegen.mir.mvalue.*
import codegen.mir.mcontrol.*
import backend.ir.irvalue.VType
import backend.ir.ir.Predicate

trait MInstr extends MValue

case class MAdd(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType       = dest.vtype
  override def toString(): String = "add " + lhs + ", " + rhs

case class MSub(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType       = dest.vtype
  override def toString(): String = "sub " + lhs + ", " + rhs

case class MMul(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType       = dest.vtype
  override def toString(): String = "mul " + lhs + ", " + rhs

case class MDiv(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType       = dest.vtype
  override def toString(): String = "div " + lhs + ", " + rhs

case class MCall(fn: MLabel) extends MInstr:
  override def vtype: VType       = VType.Unit
  override def toString(): String = "call " + fn

case class MCmp(pred: Predicate, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType       = VType.Bool
  override def toString(): String = "cmp " + pred + " " + lhs + ", " + rhs

case class MBr(cond: Predicate, tbranch: MLabel, fbranch: MLabel) extends MInstr:
  override def vtype: VType       = VType.Unit
  override def toString(): String = "br " + tbranch + ", " + fbranch

case class MMov(lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType =
    rhs.vtype
  override def toString(): String = "mov " + lhs + ", " + rhs

case class MJmp(label: MLabel) extends MInstr:
  override def vtype: VType       = VType.Unit
  override def toString(): String = "jmp " + label

case class MRet() extends MInstr:
  override def vtype: VType       = VType.Unit
  override def toString(): String = "ret"
