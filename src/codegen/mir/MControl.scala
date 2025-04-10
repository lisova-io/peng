package codegen.mir.mcontrol

import codegen.mir.mvalue.*
import scala.collection.mutable.HashMap
import codegen.mir.minstr.*
import backend.ir.irvalue.VType

class MBBlock(name: MLabel, var instrs: Vector[MInstr]) extends MValue:
  override def vtype: VType = VType.Unit
  override def toString: String =
    if instrs.nonEmpty then
      val sep = System.lineSeparator() + " "
      s".$name:" + sep + instrs.mkString(sep) + System.lineSeparator()
    else s".$name"

class MFunction(
    blocks: Vector[MBBlock],
    name: MLabel,
    rtype: VType,
    args: List[MValue],
    blockMap: HashMap[MLabel, MBBlock],
) extends MValue:
  override def vtype: VType = rtype
  override def toString: String =
    s"$name" + System.lineSeparator()
      + blocks.mkString

class Module(fns: HashMap[String, MFunction]):
  override def toString: String =
    fns.map((_, fns) => fns).mkString
