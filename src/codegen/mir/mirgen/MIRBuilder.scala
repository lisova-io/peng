package codegen.mir.mirgen.mirbuilder

import codegen.mir.mcontrol.*
import codegen.mir.mvalue.*
import codegen.mir.minstr.*
import backend.ir.irvalue.VType
import scala.collection.mutable.HashMap

class MBlockBuilder:
  var instrs: Vector[MInstr] = Vector()
  var name: MLabel           = MLabel(String())
  def build: MBBlock         = MBBlock(name, instrs)
  def reset: MBlockBuilder =
    instrs = Vector()
    name = MLabel(String())
    this
  def takeRight(m: Int): Vector[MInstr] =
    instrs.takeRight(m)
  def last: MInstr =
    instrs.last
  def add(minstrs: Vector[MInstr]): MBlockBuilder =
    instrs = instrs ++ minstrs
    this
  def add(instr: MInstr): MBlockBuilder =
    instrs :+= instr
    this
  def set(name: MLabel): MBlockBuilder =
    this.name = name
    this

class MFnBuilder:
  var blocks: Vector[MBBlock]        = Vector()
  var name: MLabel                   = MLabel(String())
  var tp: VType                      = VType.Unit
  var args: List[MValue]             = List()
  var bmap: HashMap[MLabel, MBBlock] = HashMap()
  def build: MFunction =
    MFunction(blocks, name, tp, args, bmap)
  def reset: MFnBuilder =
    blocks = Vector()
    name = MLabel(String())
    tp = VType.Unit
    args = List()
    bmap = HashMap()
    this

  def add(b: MBBlock): MFnBuilder =
    bmap.addOne((b.name, b))
    blocks :+= b
    this
  def set(name: MLabel): MFnBuilder =
    this.name = name
    this
  def set(tp: VType): MFnBuilder =
    this.tp = tp
    this
  def add(arg: MValue): MFnBuilder =
    args :+= arg
    this
