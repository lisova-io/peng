package backend.irvalue

enum VType:
  case IInt
  case IBool
  case Var
  case Fn
  case BBlock

abstract class Value:
  def vtype: VType
  def isConst: Boolean

case class ImmInt(input: Int) extends Value:
  override def vtype: VType     = VType.IInt
  override def isConst: Boolean = true

case class ImmBool(input: Boolean) extends Value:
  override def vtype: VType     = VType.IBool
  override def isConst: Boolean = true

case class Var(input: String) extends Value:
  override def vtype: VType     = VType.Var
  override def isConst: Boolean = false
