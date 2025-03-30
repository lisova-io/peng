package backend.opt.passes.trivialdce

import backend.opt.passmanager.LocalPass
import backend.ir.control.BasicBlock
import backend.ir.control.Function
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import backend.ir.irvalue.Value
import backend.opt.passmanager.GlobalPass
import backend.ir.irvalue.Var
import backend.ir.ir._

class TrivialDCE extends GlobalPass:
  private val values: HashSet[Value] = HashSet()
  private var changed: Boolean       = true

  private def processArgValue(value: Value): Unit =
    value match
      case Var(_, _) => values.remove(value)
      case _         => return

  private def processDestValue(instr: Instr): Unit =
    instr match
      case c: NonVoid => values.add(c.getDest)
      case _          => return

  private def processInstr(instr: Instr): Unit =
    instr match
      case bin: BinaryOp =>
        val (first, second) = bin.getArgs
        processArgValue(first)
        processArgValue(second)

      case un: UnaryOp =>
        val arg = un.getArg
        processArgValue(arg)
      case vararg: VarOp =>
        val argList = vararg.getArgs
        argList.foreach(processArgValue(_))
      case _ => ()
    processDestValue(instr)

  private def processBlock(block: BasicBlock) =
    for (instr <- block.instrs) do processInstr(instr)

  private def postProcessBlock(block: BasicBlock) =
    val oldLen = block.instrs.length
    block.instrs = block.instrs.filterNot({
      _ match
        case instr: NonVoid =>
          values.contains(instr.getDest)
        case _ => false
    })
    changed = block.instrs.length != oldLen
  override def pass(fn: Function): Function =
    while (changed) do
      changed = false
      for (block <- fn.blocks) do processBlock(block)
      for (block <- fn.blocks) do postProcessBlock(block)
    fn
