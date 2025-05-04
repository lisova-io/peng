package codegen.mir.register

import codegen.mir.mvalue.*
import backend.ir.irvalue.VType

trait Register extends MValue

case class AMD64Register(reg: amdReg) extends Register:
  def vtype: VType = VType.I32
  override def toString(): String =
    reg.toString()

enum amdReg:
  case rax
  case rbx
  case rsp
  case rdi
  case rsi
  case rdx
  case rcx
  case r8
  case r9
  override def toString(): String =
    this match
      case amdReg.rax => "rax"
      case amdReg.rbx => "rbx"
      case amdReg.rcx => "rcx"
      case amdReg.rdx => "rdx"
      case amdReg.rsp => "rsp"
      case amdReg.rdi => "rdi"
      case amdReg.rsi => "rsi"
      case amdReg.r8  => "r8"
      case amdReg.r9  => "r9"
