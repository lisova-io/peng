package backend.opt.passmanager

import scala.collection.mutable.HashMap
import backend.ir.control._

type Program = HashMap[String, Function]

trait PassManager:
  def addPass(pass: Pass): PassManager
  def perform: Program

trait Pass

trait LocalPass extends Pass:
  def pass(block: BasicBlock): BasicBlock
trait GlobalPass extends Pass:
  def pass(fn: Function): Function

class DefaultManager(program: Program) extends PassManager:
  var current_program    = program
  var passes: List[Pass] = List()
  def addPass(pass: Pass): DefaultManager =
    passes :+= pass
    this
  private def localPass(pass: LocalPass): Program =
    program
      .map((name, fn) => {
        val new_blocks = fn.blocks.map(pass.pass(_))
        (name, fn.copy(blocks = new_blocks))
      })

  private def globalPass(pass: GlobalPass): Program =
    program
      .map((name, fn) => {
        (name, pass.pass(fn))
      })

  private def performPass(pass: Pass): Program =
    pass match
      case lp: LocalPass  => localPass(lp)
      case gp: GlobalPass => globalPass(gp)
      case _              => ???
  def perform: Program =
    for (pass <- passes) do current_program = performPass(pass)
    current_program
