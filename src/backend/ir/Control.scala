package backend.ir.control

import backend.ir.ir.*
import backend.ir.irvalue.*
import backend.irgen.irbuilder.*
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set
import scala.annotation.internal.RuntimeChecked
import scala.util.boundary, boundary.break
import scala.collection.mutable.Stack
import backend.ir.renamer.Renamer
import scala.collection.mutable.ArrayBuffer
import backend.ir.dominators.*
import backend.ir.ssaconverter.SSAConverter

trait ControlFlow

class Program(val fns: HashMap[String, Function]) extends ControlFlow:
  override def toString: String =
    fns.mkString(System.lineSeparator())
  def toSSA: Program =
    fns.foreach((_, fn) => fn.ssa)
    this

case class BasicBlock(name: Label, var instrs: Vector[Instr]) extends Value with ControlFlow:
  def addInstruction(instr: Instr): Unit    = instrs :+= instr
  val phis: HashMap[Var, (Phi, Set[Label])] = HashMap()
  override def vtype: VType                 = VType.Unit // idk

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)

  def stMatch(v: Val): Boolean =
    ???
  def varDefined(v: Var): Boolean =
    boundary:
      for instr <- instrs do
        instr match
          case variable: NonVoid => if variable.getDest == v then break(true)
          case _                 => ()
      false

  def insertPhi(v: Var, l: Label): Unit =
    phis.get(v) match
      case Some((phi, set)) =>
        phi.add(v, l)
        phis(v) = (phi, set + l)
      case None =>
        val phi = Phi(v, List(v), List(l))
        instrs +:= phi
        phis.addOne(v -> (phi, Set(l)))

  def renamePhi(l: Label, stacks: HashMap[Var, Stack[Var]]): Unit =
    instrs = instrs.map(i =>
      i match
        case phi: Phi => phi.rename(l, stacks)
        case i        => i
    )

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
    val blocks: Vector[BasicBlock],
    rtype: VType,
    val name: Label,
    args: List[Value],
    blockMap: HashMap[Label, BasicBlock],
    blockPreds: HashMap[Label, Set[Label]],
    vars: HashMap[Var, Set[Label]],
) extends Value
    with ControlFlow
    with Dominators[Label]
    with SSAConverter[Var, Label, BasicBlock]:

  override def accept[Return](visitor: ValueVisitor[Return]): Return =
    visitor.visit(this)
  // for dominators
  override def all: Set[Label] = blocks.map(block => block.name).toSet
  def entry: Label = blockMap.get(name) match
    case Some(value) => value.name
    case None =>
      println("entry block doesn't have the same name as fn")
      ???
  def preds: Label => Set[Label] = blockPreds

  override def closest(
      sdoms: HashMap[Label, Set[Label]],
      block: Label,
  ): Label =
    def closer(b1: Label, b2: Label): Label =
      if sdoms(b1).contains(b2) then b1
      else b2
    val sds = sdoms(block)
    sds.foldLeft(sds.head)((acc, b) => closer(acc, b))

  override def vtype: VType = rtype

  private var ssad = false

  private var varDefs: HashMap[Var, Label] = HashMap()

  override def toString: String =
    s"$rtype $name(" + args.mkString(", ")
      + "): {" + System.lineSeparator()
      + blocks.mkString + "}"

  def stMatch(v: Val): Boolean =
    ???

  def vertexes: HashMap[Label, BasicBlock] = blockMap
  def insertPhi(block: BasicBlock, variable: Var, from: Label): Unit =
    block.insertPhi(variable, from)
  def getPhis(block: BasicBlock, variable: Var): Option[Set[Label]] =
    block.phis.get(variable).map((p, set) => set)

  def rename =
    val dtree = domTree

    val stacks: HashMap[Var, Stack[Var]] =
      val vars = for
        block <- blocks
        instr <- block.instrs if instr.isInstanceOf[NonVoid]
      yield
        val nv = instr.asInstanceOf[NonVoid]
        val v  = nv.getDest
        v -> Stack(v)
      vars.to(HashMap)

    val renamer                      = Renamer()
    val newVars: HashMap[Var, Label] = HashMap()
    renameBlock(blockMap(name))
    varDefs = newVars
    def renameBlock(b: BasicBlock): Unit =
      val pushed: ArrayBuffer[Var] = ArrayBuffer()
      b.instrs = b.instrs.map(instr =>
        instr match
          case nv: NonVoid => pushed.addOne(nv.getDest)
          case _           => ()
        val newInstr = renamer.rename(instr, stacks, b.name)
        newInstr match
          case nv: NonVoid =>
            val dest = nv.getDest
            newVars.addOne(dest -> b.name)
          case _ => ()
        newInstr
      )
      for suc <- b.getSuccessors do blockMap(suc).renamePhi(b.name, stacks)
      if dtree.contains(b.name) then dtree(b.name).foreach(l => renameBlock(blockMap(l)))
      for v <- pushed do stacks(v).pop()

  // this one is for evaluator purposes only :)
  def destroySSA: Unit =
    def copy(toCopy: Var, v: Var, l: Label) =
      val block = blockMap(l)
      block.instrs = block.instrs.init.appended(Mov(toCopy, v)).appended(block.instrs.last)
    blocks.map(b =>
      b.instrs.map(i =>
        i match
          case Phi(dest, vals, defined) =>
            vals.zip(defined).map((v, l) => copy(dest.asInstanceOf[Var], v, l))
          case _ => ()
      )
    )
    blocks.map(b => b.instrs = b.instrs.filterNot(i => i.isInstanceOf[Phi]))

  def ssa: Function =
    if !ssad then
      ssaFn
      ssad = true
    rename
    this
