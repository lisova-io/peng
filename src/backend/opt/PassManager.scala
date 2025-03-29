package backend.opt.passmanager

import scala.collection.mutable.HashMap
import backend.ir.control._

trait PassManager

trait Pass

trait LocalPass extends Pass:
  def pass(block: BasicBlock): BasicBlock
trait GlobalPass extends Pass:
  def pass(fn: Function): Function

class DefaultManager(program: HashMap[String, Function]) extends PassManager:
  var current_program    = program
  var passes: List[Pass] = List()
  def addPass(pass: Pass): DefaultManager =
    passes :+= pass
    this
  private def localPass(pass: LocalPass): HashMap[String, Function] =
    program
      .map((name, fn) => {
        val new_blocks = fn.blocks.map(pass.pass(_))
        (name, fn.copy(blocks = new_blocks))
      })

  private def globalPass(pass: GlobalPass): HashMap[String, Function] =
    program
      .map((name, fn) => {
        (name, pass.pass(fn))
      })

  private def performPass(pass: Pass): HashMap[String, Function] =
    pass match
      case lp: LocalPass  => localPass(lp)
      case gp: GlobalPass => globalPass(gp)
      case _              => ???
  def perform: HashMap[String, Function] =
    for (pass <- passes) do current_program = performPass(pass)
    current_program
