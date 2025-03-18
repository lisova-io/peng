package backend.irvalue

abstract class Value

sealed case class ImmInt(input: Int) extends Value

sealed case class ImmBool(input: Boolean) extends Value

sealed case class Var(input: String) extends Value
