import frontend.errorprint._
import frontend.parse._
import frontend.ast._

@main def main() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input  = source.getLines mkString "\n"
  // parse(input) match
  //   case Left(err)  => printErrs(input, List(err))
  //   case Right(ast) => ast.foreach((_, d) => println(d))
}
