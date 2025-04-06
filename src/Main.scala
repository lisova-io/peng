import frontend.diagnostics.{containsErrors, containsNotes, containsWarnings, printDiagnostics}
import frontend.ast.printAST
import frontend.sema.SemaResult

import backend.irgen.asttranslator.*
import backend.opt.passmanager.PassManager
import backend.opt.passmanager.DefaultManager
import backend.opt.passes.trivialdce.TrivialDCE
import backend.ir.irvalue.ImmInt
import backend.opt.passsetup.OptLevel

import overseer.DebugOverseer
import overseer.DefaultOverseer

enum Mode:
  case Default
  case Debug

// TODO: get this settings from cli args
val mode     = Mode.Debug
val optLevel = OptLevel.FullOpt

@main def main(): Unit = {

  val overseer = mode match
    case Mode.Debug   => DebugOverseer
    case Mode.Default => DefaultOverseer

  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer                      = overseer.getLexer(input)
  val parser                     = overseer.getParser(lexer)
  val (decls, parserDiagnostics) = parser.parse

  printDiagnostics(input, parserDiagnostics)
  if parserDiagnostics.containsErrors then return

  val SemaResult(ast, semaDiagnostics) = overseer.getSema.run(decls)
  printDiagnostics(input, semaDiagnostics)
  if semaDiagnostics.containsErrors then return

  printAST(ast)

  // val translator = overseer.getTranslator(ast)

  // val ir = translator.gen
  // ir.foreach((_, actual) => println(actual))

  // val passmanager = overseer.getPassManager(ir, optLevel)

  // val newIR = passmanager.addPass(TrivialDCE()).perform
  // newIR.foreach((_, actual) => println(actual))
}
