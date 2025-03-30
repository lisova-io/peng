package codegen.spec

enum CallConvention:
  case RV
  case SystemV

trait MachineSpec:
  def wordBits: Int
  def wordBytes = wordBits / 8
  def stackAlign: Int
  def cconv: CallConvention
