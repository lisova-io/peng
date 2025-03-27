import frontend.diagnostics.{printDiagnostics, containsErrors, containsWarnings, containsNotes}
import frontend.lex.Lexer
import frontend.parse.Parser

@main def main(): Unit = {
  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer              = Lexer(input)
  val (ast, diagnostics) = Parser(lexer).parse

  printDiagnostics(input, diagnostics)
  if diagnostics.containsErrors then return

  ast.foreach((_, d) => println(d))
}
