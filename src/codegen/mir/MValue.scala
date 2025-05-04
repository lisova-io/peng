package codegen.mir.mvalue

import backend.ir.irvalue.VType
import backend.ir.ir.Predicate

abstract class MValue:
  def vtype: VType

trait Symbol

trait Imm extends MValue

class MLabel(name: String) extends MValue with Symbol:
  def vtype: VType                = VType.Label
  override def toString(): String = name

class MVar(input: String, vartype: VType) extends MValue:
  def vtype: VType                = vartype
  override def toString(): String = input

class MInt(input: BigInt) extends MValue with Imm:
  def vtype: VType                = VType.I32
  override def toString(): String = input.toString()

class MBool(input: Boolean) extends MValue with Imm:
  def vtype: VType                = VType.Bool
  override def toString(): String = input.toString()
