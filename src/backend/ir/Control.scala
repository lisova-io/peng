package backend.ir.control

import backend.ir.ir.*
import backend.ir.irvalue.*
import backend.irgen.irbuilder.*

trait ControlFlow

class Program(fns: Vector[Function]) extends ControlFlow

case class BasicBlock(name: String, var instrs: Vector[Instr]) extends Value with ControlFlow:
  override def vtype: VType = VType.Unit // idk
  override def toString: String =
    val sep = System.lineSeparator() + "  "
    s"%$name:" + sep + instrs.mkString(sep) + System.lineSeparator()

case class Function(blocks: Vector[BasicBlock], rtype: VType, name: String, args: List[Value])
    extends Value
    with ControlFlow:
  override def vtype: VType = rtype
  override def toString: String =
    s"$rtype $name(" + args.mkString(", ")
      + "): " + System.lineSeparator()
      + blocks.mkString
