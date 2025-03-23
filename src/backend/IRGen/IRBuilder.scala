package backend.irgen.irbuilder

import backend.control._
import backend.ir._

abstract class Builder[T]:
  def build: T
  def reset: Builder[T]

class BBuilder extends Builder[BasicBlock]:
  var instrs: Vector[Instr]
