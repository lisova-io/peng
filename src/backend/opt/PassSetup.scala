package backend.opt.passsetup

import backend.opt.passmanager.PassManager
import backend.opt.passes.trivialdce.TrivialDCE

enum OptLevel:
  case NoOpt
  case FullOpt

class PassSetup(pm: PassManager, level: OptLevel):
  private val fullOptPasses = List(TrivialDCE())
  def fullOpt(pm: PassManager): PassManager =
    fullOptPasses.foreach(pm.addPass(_))
    pm
  def configure: PassManager =
    level match
      case OptLevel.NoOpt   => pm
      case OptLevel.FullOpt => fullOpt(pm)
