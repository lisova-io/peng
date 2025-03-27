package backend.control

import backend.ir._
import backend.irvalue._
import backend.irgen.irbuilder._

trait ControlFlow

class Program(fns: Vector[Function]) extends ControlFlow

case class BasicBlock(name: String, instrs: Vector[Instr]) extends Value with ControlFlow:
  override def vtype: VType     = VType.unit // idk
  override def isConst: Boolean = false
  override def toString: String =
    s"$name:" + instrs.foldLeft((acc: String, instr: Instr) => acc + instr + "\n")

case class Function(blocks: Vector[BasicBlock], rtype: VType, name: String, args: List[Value])
    extends Value
    with ControlFlow:
  override def vtype: VType     = rtype
  override def isConst: Boolean = false
