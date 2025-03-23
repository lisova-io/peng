import frontend.errorprint.printErrs
import frontend.lex.Lexer
import frontend.parse.Parser

@main def main() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input =
    try source.mkString
    finally source.close()

  val lexer = Lexer(input)
  Parser(lexer).parse match
    case Left(errs) => printErrs(input, errs)
    case Right(ast) => ast.foreach((_, d) => println(d))
}
