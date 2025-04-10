package backend.ir.irvalue

enum VType:
  case I32
  case Bool
  case Unit
  case Label
  override def toString: String =
    this match
      case I32         => "i32"
      case Bool        => "bool"
      case Unit        => "unit"
      case VType.Label => "label"

abstract class Value:
  def vtype: VType
  def isConst: Boolean = false

case class ImmInt(input: BigInt) extends Value:
  def vtype: VType              = VType.I32
  override def isConst: Boolean = true
  override def toString: String = s"$input"

case class ImmBool(input: Boolean) extends Value:
  def vtype: VType              = VType.Bool
  override def isConst: Boolean = true
  override def toString: String = s"$input"

case class Var(val input: String, vartype: VType) extends Value:
  def vtype: VType              = vartype
  override def toString: String = s"$input"

case class Label(val name: String) extends Value:
  def vtype: VType              = VType.Label
  override def toString: String = s"$name"

case class Void() extends Value:
  def vtype: VType              = VType.Unit
  override def toString: String = "_"
