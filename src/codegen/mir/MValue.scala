package codegen.mir.mvalue

import backend.ir.irvalue.VType

abstract class MValue:
  def vtype: VType

trait Symbol

trait Imm extends MValue

class MLabel(name: String) extends MValue with Symbol:
  def vtype: VType = VType.Label

class MVar(input: String, vartype: VType) extends MValue:
  def vtype: VType = vartype

class MInt(input: BigInt) extends MValue with Imm:
  def vtype: VType = VType.I32

class MBool(input: Boolean) extends MValue with Imm:
  def vtype: VType = VType.Bool
