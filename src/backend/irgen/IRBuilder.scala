package backend.irgen.irbuilder

import backend.ir.control._
import backend.ir.ir._
import backend.ir.irvalue._
import scala.collection.mutable.HashMap

// TODO: type-safe builder for function

abstract class Builder[T]:
  def build: T
  def reset: Builder[T]

class BBuilder extends Builder[BasicBlock]:
  var instrs: Vector[Instr] = Vector()
  var name: Label           = Label(String())
  def build: BasicBlock     = BasicBlock(name, instrs)
  def reset: BBuilder =
    instrs = Vector()
    name = Label(String())
    this
  def addInstr(instr: Instr): BBuilder =
    instrs :+= instr
    this
  def setName(name: Label): BBuilder =
    this.name = name
    this

class FnBuilder extends Builder[Function]:
  var blocks: Vector[BasicBlock]           = Vector()
  var name: String                         = String()
  var args: List[Value]                    = List()
  var vtype: VType                         = VType.unit
  var blockMap: HashMap[Label, BasicBlock] = HashMap()
  def addBlock(block: BasicBlock): FnBuilder =
    blocks :+= block
    this
  def setName(name: String): FnBuilder =
    this.name = name
    this
  def addArg(arg: Value): FnBuilder =
    args :+= arg
    this
  def setType(vtype: VType): FnBuilder =
    this.vtype = vtype
    this
  def reset: FnBuilder =
    blocks = Vector()
    name = String()
    args = List()
    vtype = VType.unit
    blockMap = HashMap()
    this
  def build: Function =
    Function(blocks, vtype, name, args, blockMap)
