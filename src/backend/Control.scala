package backend.control

import backend.ir._
import backend.irvalue._

trait ControlFlow

class Program extends ControlFlow

case class BasicBlock(name: String, instrs: List[Instr]) extends Value with ControlFlow:
  override def vtype: VType     = VType.BBlock
  override def isConst: Boolean = false

case class Function(name: String, args: List[Value]) extends Value with ControlFlow:
  override def vtype: VType     = VType.Fn
  override def isConst: Boolean = false
