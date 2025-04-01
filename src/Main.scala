import frontend.diagnostics.{printDiagnostics, containsErrors, containsWarnings, containsNotes}
import frontend.lex.Lexer
import frontend.parse.Parser
import frontend.ast.printAST

import backend.irgen.asttranslator._
import backend.opt.passmanager.PassManager
import backend.opt.passmanager.DefaultManager
import backend.opt.passes.trivialdce.TrivialDCE
import backend.ir.irvalue.ImmInt
import overseer.DebugOverseer
import overseer.DefaultOverseer
import backend.opt.passsetup.OptLevel
import backend.graphviz._
import java.io.{File, FileWriter}
import java.io.PrintWriter

enum Mode:
  case Default
  case Debug

enum Arch:
  case X86

// TODO: get this settings from cli args
val mode     = Mode.Debug
val optLevel = OptLevel.FullOpt
val arch     = Arch.X86

def writeToFile(path: String, mes: String): Unit =
  val pw = new PrintWriter(new File(path))
  try
    pw.write(mes)
  finally
    pw.close()

@main def main(): Unit = {

  val overseer = mode match
    case Mode.Debug   => DebugOverseer
    case Mode.Default => DefaultOverseer

  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer = overseer.getLexer(input)

  val parser = overseer.getParser(lexer)

  val (ast, diagnostics) = parser.parse

  printDiagnostics(input, diagnostics)
  if diagnostics.containsErrors then return

  printAST(ast)

  val translator = overseer.getTranslator(ast)

  val ir = translator.gen

  print(ir)

  val gv = GraphViz.programToGV(ir)
  writeToFile("program.dot", gv.toString)

  val passmanager = overseer.getPassManager(ir, optLevel)

  // val newIR = passmanager.addPass(TrivialDCE()).perform
  // newIR.foreach((_, actual) => println(actual))
}
