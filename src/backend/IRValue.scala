package backend.irvalue

<<<<<<< HEAD
enum VType:
  case i32
  case bool
  case unit

abstract class Value:
  def vtype: VType
  def isConst: Boolean

case class ImmInt(input: Int) extends Value:
  override def vtype: VType       = VType.i32
  override def isConst: Boolean   = true
  override def toString(): String = s"i32 $input"

case class ImmBool(input: Boolean) extends Value:
  override def vtype: VType       = VType.bool
  override def isConst: Boolean   = true
  override def toString(): String = s"bool $input"

case class Var(input: String, vartype: VType) extends Value:
  override def vtype: VType       = vartype
  override def isConst: Boolean   = false
  override def toString(): String = s" $input"
<<<<<<< HEAD
=======
abstract class Value

sealed case class ImmInt(input: Int) extends Value

sealed case class ImmBool(input: Boolean) extends Value

sealed case class Var(input: String) extends Value
>>>>>>> faf60da (backending all over compiler)
=======
>>>>>>> 6417e3e (logging and sheise)
