package backend.ir.control

import backend.ir.ir._
import backend.ir.irvalue._
import backend.irgen.irbuilder._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set
import scala.annotation.internal.RuntimeChecked
import scala.util.boundary, boundary.break
import scala.collection.mutable.Stack

trait ControlFlow

var counter: Int = 0

class Program(val fns: HashMap[String, Function]) extends ControlFlow:
  override def toString: String =
    fns.mkString(System.lineSeparator())

case class BasicBlock(name: Label, var instrs: Vector[Instr]) extends Value with ControlFlow:
  def addInstruction(instr: Instr): Unit = instrs :+= instr
  val phis: HashMap[Var, Phi]            = HashMap()
  override def vtype: VType              = VType.unit // idk
  def varDefined(v: Var): Boolean =
    boundary:
      for instr <- instrs do
        instr match
          case variable: NonVoid => if variable.getDest == v then break(true)
          case _                 => ()
      false

  def insertPhi(v: Var, l: Label): Unit =
    phis.get(v) match
      case Some(phi) =>
        phi.add(v, l)
      case None =>
        val phi = Phi(v, List(v), List(l))
        instrs +:= phi
        phis.addOne(v -> phi)

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
    blockPreds: HashMap[Label, Set[Label]],
    vars: HashMap[Var, Set[Label]]
) extends Value
    with ControlFlow:

  override def vtype: VType = rtype

  override def toString: String =
    s"$rtype $name(" + args.mkString(", ")
      + "): {" + System.lineSeparator()
      + blocks.mkString + "}"

  private def closest(
      sdoms: HashMap[Label, Set[Label]],
      block: Label
  ): Label =
    def closer(b1: Label, b2: Label): Label =
      if sdoms(b1).contains(b2) then b1
      else b2
    val sds = sdoms(block)
    sds.foldLeft(sds.head)((acc, b) => closer(acc, b))

  def insertPhi =
    val stack: Stack[Label] = Stack()
    val df                  = dominationFrontier
    val sdom                = strictDominators
    for (v, defs) <- vars do
      for labelDefs <- defs do stack.push(labelDefs)
      while !stack.isEmpty do
        val defLabel = stack.pop

        if df contains defLabel then
          for domfLabel <- df(defLabel) do
            val domfBlock = blockMap(domfLabel)
            if !domfBlock.phis.contains(v) then
              domfBlock.insertPhi(v, defLabel)
              stack.push(domfLabel)
              if blockPreds.contains(domfLabel) && sdom.contains(domfLabel) then
                for bp <- blockPreds(domfLabel) do
                  if sdom(domfLabel).contains(bp) then
                    if blockMap(bp).varDefined(v) then domfBlock.insertPhi(v, bp)

    blocks.foreach(block =>
      block.instrs = block.instrs.filterNot(instr =>
        instr match
          case Phi(dest, vals, defined) => if vals.length < 2 then true else false
          case _                        => false
      )
    )

  def domTree: HashMap[Label, List[Label]] =
    def addToMap[K, V](key: K, value: V, m: HashMap[K, List[V]]) =
      if m.contains(key) then m(key) :+= value
      else m.addOne(key -> List(value))
    val dtree: HashMap[Label, List[Label]] = HashMap()
    val sdoms                              = strictDominators
    for (block, sds) <- sdoms do
      if !sds.isEmpty then
        if sds.size == 1 then addToMap(sds.head, block, dtree)
        else addToMap(closest(sdoms, block), block, dtree)
    dtree

  def strictDominators: HashMap[Label, Set[Label]] =
    val doms = dominators
    doms.map((b, set) => b -> (set - b))

  def immediateDominators: HashMap[Label, Label] =
    val sdoms = strictDominators
    // it is equivalent to (block, set) <- sdoms
    // but it's funny asf
    for block -> set <- sdoms if !sdoms(block).isEmpty
    yield block -> closest(sdoms, block)

  def dominationFrontier: HashMap[Label, Set[Label]] =
    def addToMap[K, V](key: K, value: V, m: HashMap[K, Set[V]]) =
      if m.contains(key) then m(key) = m(key) + value
      else m.addOne(key, Set(value))
    val idom                           = immediateDominators
    val df: HashMap[Label, Set[Label]] = HashMap()
    for block <- blocks do
      if idom.contains(block.name) then
        val blockIDom = idom(block.name)
        for pred <- blockPreds(block.name) do
          var holder = pred
          while !holder.equals(blockIDom) do
            addToMap(holder, block.name, df)
            holder = idom(holder)
    df

  def dominators: HashMap[Label, Set[Label]] =
    val all: Set[Label] = blocks.map(block => block.name).toSet
    val dom: HashMap[Label, Set[Label]] =
      blocks.map(block => block.name -> all).to(HashMap)
    val entryBlock = blockMap.get(name) match
      case Some(value) => value
      case None =>
        println("entry block doesn't have the same name as fn")
        throw RuntimeException()
    dom(entryBlock.name) = Set(entryBlock.name)
    var changed = true
    while changed do
      changed = false
      for block <- blocks if block != entryBlock do
        var temp = all;

        for pred <- blockPreds(block.name) do temp = temp.intersect(dom(pred))
        temp += block.name
        if !dom(block.name).equals(temp) then
          changed = true
          dom(block.name) = temp;
    dom
