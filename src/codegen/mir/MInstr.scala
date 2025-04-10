package codegen.mir.minstr

import codegen.mir.mvalue.*
import codegen.mir.mcontrol.*
import backend.ir.irvalue.VType
import backend.ir.ir.Predicate

trait MInstr extends MValue

class MAdd(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType = dest.vtype

class MSub(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType = dest.vtype

class MMul(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType = dest.vtype

class MDiv(dest: MValue, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType = dest.vtype

class MCall(dest: MValue, fn: MLabel, args: List[MValue]) extends MInstr:
  override def vtype: VType = dest.vtype

class MCmp(dest: MValue, pred: Predicate, lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType = dest.vtype

class MBr(cond: MValue, tbranch: MLabel, fbranch: MLabel) extends MInstr:
  override def vtype: VType = cond.vtype

class MMov(lhs: MValue, rhs: MValue) extends MInstr:
  override def vtype: VType =
    rhs.vtype

class MJmp(label: MLabel) extends MInstr:
  override def vtype: VType = VType.Unit

class MRet(ret: MValue) extends MInstr:
  override def vtype: VType = ret.vtype
