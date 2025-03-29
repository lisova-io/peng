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

enum Mode:
  case Default
  case Debug

val mode = Mode.Debug

@main def main(): Unit = {
  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer              = Lexer(input)
  val (ast, diagnostics) = Parser(lexer).parse

  printDiagnostics(input, diagnostics)
  if diagnostics.containsErrors then return

  // printAST(ast)
  val overseer = mode match
    case Mode.Debug   => DebugOverseer
    case Mode.Default => DefaultOverseer

  val translator = overseer.getTranslator(ast)

  val ir = translator.gen

  val passmanager = overseer.getPassManager(ir)

  val newIR = passmanager.addPass(TrivialDCE()).perform
  // newIR.foreach((_, actual) => println(actual))
}
