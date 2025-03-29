import frontend.diagnostics.{printDiagnostics, containsErrors, containsWarnings, containsNotes}
import frontend.lex.Lexer
import frontend.parse.Parser
import frontend.ast.printAST
import backend.irgen.asttranslator._
import backend.opt.passmanager.PassManager
import backend.opt.passmanager.DefaultManager
import backend.opt.passes.trivialdce.TrivialDCE

@main def main(): Unit = {
  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer              = Lexer(input)
  val (ast, diagnostics) = Parser(lexer).parse

  printDiagnostics(input, diagnostics)
  if diagnostics.containsErrors then return

  printAST(ast)

  val ir = DefaultTranslator(ast).gen
  println("BEFORE PASSES:")
  ir.foreach((_, actual) => println(actual))
  println("================================")
  val newIR = DefaultManager(ir).addPass(TrivialDCE()).perform
  println("AFTER:")
  newIR.foreach((_, actual) => println(actual))

}
