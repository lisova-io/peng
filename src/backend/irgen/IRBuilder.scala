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
  var blocks: Vector[BasicBlock]             = Vector()
  var name: Label                            = Label(String())
  var args: List[Value]                      = List()
  var vtype: VType                           = VType.Unit
  var blockMap: HashMap[Label, BasicBlock]   = HashMap()
  var labelPreds: HashMap[Label, Set[Label]] = HashMap()
  var vars: HashMap[Var, Set[Label]]         = HashMap()
  def addVar(v: Var, l: Label): FnBuilder =
    if vars.contains(v) then vars(v) += l
    else vars.addOne(v -> Set(l))
    this
  def addBlock(block: BasicBlock): FnBuilder =
    blocks :+= block
    blockMap.addOne(block.name -> block)
    if !labelPreds.contains(block.name) then labelPreds.addOne(block.name -> Set())
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
    labelPreds = HashMap()
    this
  def addPred(succ: Label, pred: Label): FnBuilder =
    if labelPreds.contains(succ) then labelPreds(succ) += pred
    else labelPreds.addOne(succ -> Set(pred))
    this
  def build: Function =
    // val blocksPred: HashMap[BasicBlock, Set[BasicBlock]] =
    //   labelPreds
    //     .map((label, set) => blockMap(label) -> set.map(label => blockMap(label)))
    //     .to(HashMap)
    //
    // val varsBlocks: HashMap[Var, Set[BasicBlock]] =
    //   vars.map((v, labels) => v -> labels.map(l => blockMap(l)))
    Function(blocks, vtype, name, args, blockMap, labelPreds, vars)
