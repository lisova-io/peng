package backend.ir.control

import backend.ir.ir._
import backend.ir.irvalue._
import backend.irgen.irbuilder._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set
import scala.annotation.internal.RuntimeChecked

trait ControlFlow

class Program(val fns: HashMap[String, Function]) extends ControlFlow:
  override def toString: String =
    fns.mkString(System.lineSeparator())

case class BasicBlock(name: Label, var instrs: Vector[Instr]) extends Value with ControlFlow:
  def addInstruction(instr: Instr): Unit = instrs :+= instr
  override def vtype: VType              = VType.unit // idk
  def getSuccessors: List[Label] =
    instrs.last match
      case Jmp(label)                 => List(label)
      case Br(cond, tbranch, fbranch) => List(tbranch, fbranch)
      case Ret(ret)                   => List()
      case _                          => ???
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
    blockMap: HashMap[Label, BasicBlock],
    blockPreds: HashMap[BasicBlock, Set[BasicBlock]]
) extends Value
    with ControlFlow:

  override def vtype: VType = rtype

  override def toString: String =
    s"$rtype $name(" + args.mkString(", ")
      + "): {" + System.lineSeparator()
      + blocks.mkString + "}"

  private def closest(
      sds: Set[BasicBlock],
      sdoms: HashMap[BasicBlock, Set[BasicBlock]],
      block: BasicBlock
  ): BasicBlock =
    def closer(b1: BasicBlock, b2: BasicBlock): BasicBlock =
      if sdoms(b1).contains(b2) then b1
      else b2
    sds.foldLeft(sds.head)((acc, b) => closer(acc, b))

  def domTree: HashMap[BasicBlock, List[BasicBlock]] =
    def addToMap[K, V](key: K, value: V, m: HashMap[K, List[V]]) =
      if m.contains(key) then m(key) :+= value
      else m.addOne(key -> List(value))
    val dtree: HashMap[BasicBlock, List[BasicBlock]] = HashMap()
    val sdoms                                        = strictDominators
    for (block, sds) <- sdoms do
      if !sds.isEmpty then
        if sds.size == 1 then addToMap(sds.head, block, dtree)
        else addToMap(closest(sds, sdoms, block), block, dtree)
    dtree

  def strictDominators: HashMap[BasicBlock, Set[BasicBlock]] =
    val doms = dominators
    doms.map((b, set) => b -> (set - b))

  def dominators: HashMap[BasicBlock, Set[BasicBlock]] =
    val all: Set[BasicBlock] = blocks.toSet
    val dom: HashMap[BasicBlock, Set[BasicBlock]] =
      blocks.map(block => block -> all).to(HashMap)
    val entryBlock = blockMap.get(name) match
      case Some(value) => value
      case None =>
        println("entry block doesn't have the same name as fn")
        throw RuntimeException()
    dom(entryBlock) = Set(entryBlock)
    var changed = true
    while changed do
      changed = false
      for block <- blocks if block != entryBlock do
        var temp = all;

        for pred <- blockPreds(block) do temp = temp.intersect(dom(pred))
        temp += block
        if !dom(block).equals(temp) then
          changed = true
          dom(block) = temp;
    dom
