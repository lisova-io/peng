package codegen.regalloc

import codegen.mir.register.Register

trait RegPool:
  val pool: Array[Register]

trait RegisterAllocator:
  val rpool: RegPool
  def ralloc: Register
  def rfree(r: Register): Unit
