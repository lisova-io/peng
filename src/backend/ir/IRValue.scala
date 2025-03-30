package backend.ir.irvalue

enum VType:
  case i32
  case bool
  case unit
  override def toString: String =
    this.ordinal match
      case 0 => "i32"
      case 1 => "bool"
      case 2 => "void"

abstract class Value:
  def vtype: VType
  def isConst: Boolean = false

case class ImmInt(input: BigInt) extends Value:
  def vtype: VType              = VType.i32
  override def isConst: Boolean = true
  override def toString: String = s"$input"

case class ImmBool(input: Boolean) extends Value:
  def vtype: VType              = VType.bool
  override def isConst: Boolean = true
  override def toString: String = s"$input"

case class Var(input: String, vartype: VType) extends Value:
  def vtype: VType              = vartype
  override def toString: String = s"$input"

case class Label(name: String) extends Value:
  def vtype: VType              = VType.unit
  override def toString: String = s"$name"

case class Void() extends Value:
  def vtype: VType              = VType.unit
  override def toString: String = "_"
