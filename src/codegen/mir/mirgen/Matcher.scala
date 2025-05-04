package codegen.mir.mirgen.matcher

import backend.ir.irvalue.*
import backend.ir.ir.*
import codegen.mir.mvalue.*
import codegen.mir.minstr.*

// matcher: Type tryMatch -> lambda

enum Val:
  case Add(dest: Val, lhs: Val, rhs: Val)
  case Sub(dest: Val, lhs: Val, rhs: Val)
  case I32
  case Bool

  def tryMatch(lambda: Value => MValue): Value => Option[MValue] =
    (v: Value) =>
      if matchValue(this, v) then Some(lambda(v))
      else None

def matchValue(vt: Val, v: Value): Boolean = ???

val madd = Val.Add(Val.I32, Val.I32, Val.I32) tryMatch { (v: Value) =>
  val (dest, lhs, rhs) = v match
    case Add(dest, lhs, rhs) => (dest, lhs, rhs)
    case _                   => ???
  // we emit MachineAdd(i32, i32, i32)
  ???
}
