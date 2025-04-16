package driver

import diagnostics.Diagnostic
import diagnostics.{containsErrors, containsNotes, containsWarnings, printDiagnostics}

import frontend.ast.printAST
import frontend.sema.SemaResult
import frontend.diagnostics.given

import backend.irgen.asttranslator.*
import backend.opt.passsetup.OptLevel

import overseer.DebugOverseer
import overseer.DefaultOverseer

enum Input:
  case SourceFile(val name: String)

// trait Action[-InT, +OutT]:
//   private var children: List[Action[OutT, Any]] = Nil
//   def execute(input: InT): Either[Diagnostic, OutT]

// private class ActionGraphNode[-InT, +OutT](
//     val value: Action[InT, OutT],
//     val children: List[ActionGraphNode[OutT, Any]],
// ):
//   def addChild(child: Action[OutT, Any]): ActionGraphNode[InT, OutT] = ActionGraphNode(
//     value,
//     children :+ ActionGraphNode(child, Nil),
//   )

// private class ActionGraph(root: ActionGraphNode = ActionGraphNode()) {}

enum Mode:
  case Default
  case Debug

object Driver {
  // private def parseArgs(args: Seq[String]): Pipeline = ???

  // private def execute(pipelines: Pipeline): Unit = ???

  def main(args: String*): Unit = {
    // val overseer = mode match
    //   case Mode.Debug   => DebugOverseer
    //   case Mode.Default => DefaultOverseer

    val overseer = DebugOverseer

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
  // val g = parseArgs(args)
  // execute(g)
}

// @main def main(args: String*): Unit = {
//   // val overseer = mode match
//   //   case Mode.Debug   => DebugOverseer
//   //   case Mode.Default => DefaultOverseer

//   val overseer = DebugOverseer

//   val source = scala.io.Source.fromFile("input.txt")
//   val input =
//     try source.mkString
//     finally source.close()

//   val lexer                      = overseer.getLexer(input)
//   val parser                     = overseer.getParser(lexer)
//   val (decls, parserDiagnostics) = parser.parse

//   printDiagnostics(input, parserDiagnostics)
//   if parserDiagnostics.containsErrors then return

//   val SemaResult(ast, semaDiagnostics) = overseer.getSema.run(decls)
//   printDiagnostics(input, semaDiagnostics)
//   if semaDiagnostics.containsErrors then return

//   printAST(ast)

//   // val translator = overseer.getTranslator(ast)

//   // val ir = translator.gen
//   // ir.foreach((_, actual) => println(actual))

//   // val passmanager = overseer.getPassManager(ir, optLevel)

//   // val newIR = passmanager.addPass(TrivialDCE()).perform
//   // newIR.foreach((_, actual) => println(actual))
// }
