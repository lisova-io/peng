package backend.control

import backend.ir._
import backend.irvalue._

sealed class BasicBlock(name: String, instrs: List[Instr]) extends Value

sealed class Function(name: String, args: List[Value])
