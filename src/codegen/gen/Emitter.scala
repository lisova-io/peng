package codegen.gen.emitter

import scala.collection.mutable.ArrayBuffer
import codegen.mir.mvalue.*
import codegen.mir.mcontrol.*
import codegen.mir.minstr.*

class NaiveAMDEmitter:
  val data: ArrayBuffer[String] = ArrayBuffer()
  val code: ArrayBuffer[String] = ArrayBuffer()

  def emit(v: MValue): Unit = ???
