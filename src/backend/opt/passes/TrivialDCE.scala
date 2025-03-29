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
  val values: HashSet[Value] = HashSet()

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
    block.instrs = block.instrs.filterNot({
      _ match
        case instr: NonVoid =>
          values.contains(instr.getDest)
        case _ => false
    })
  override def pass(fn: Function): Function =
    for (block <- fn.blocks) do processBlock(block)
    for (block <- fn.blocks) do postProcessBlock(block)
    println(fn)
    fn
