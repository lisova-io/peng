package backend.ir.control

import backend.ir.ir._
import backend.ir.irvalue._
import backend.irgen.irbuilder._
import scala.collection.mutable.HashMap

trait ControlFlow

class Program(val fns: HashMap[String, Function]) extends ControlFlow:
  override def toString: String =
    fns.mkString

case class BasicBlock(name: Label, var instrs: Vector[Instr]) extends Value with ControlFlow:
  def addInstruction(instr: Instr): Unit = instrs :+= instr
  override def vtype: VType              = VType.unit // idk
  override def toString: String =
    if instrs.nonEmpty then
      val sep = System.lineSeparator() + "  "
      s"$name:" + sep + instrs.mkString(sep) + System.lineSeparator()
    else s"$name:" + System.lineSeparator()

case class Function(
    blocks: Vector[BasicBlock],
    rtype: VType,
    name: Label,
    args: List[Value],
    blockMap: HashMap[Label, BasicBlock]
) extends Value
    with ControlFlow:
  override def vtype: VType = rtype
  override def toString: String =
    s"$rtype $name(" + args.mkString(", ")
      + "): {" + System.lineSeparator()
      + blocks.mkString + "}"
