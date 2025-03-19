package codegen.mvalue

abstract class MValue

abstract class Register extends MValue:
  def name: String
  def isReal: Boolean
  def isVirtual: Boolean

abstract class Imm extends MValue

abstract class Label extends MValue

abstract class Memory extends MValue
