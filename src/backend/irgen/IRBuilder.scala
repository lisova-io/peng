package backend.irgen.irbuilder

import backend.ir.control.*
import backend.ir.ir.*
import backend.ir.irvalue.*
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
  var name: Label                          = Label(String())
  var args: List[Value]                    = List()
  var vtype: VType                         = VType.Unit
  var blockMap: HashMap[Label, BasicBlock] = HashMap()
  def addBlock(block: BasicBlock): FnBuilder =
    blocks :+= block
    this
  def setName(name: Label): FnBuilder =
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
    name = Label(String())
    args = List()
    vtype = VType.Unit
    blockMap = HashMap()
    this
  def build: Function =
    Function(blocks, vtype, name, args, blockMap)
