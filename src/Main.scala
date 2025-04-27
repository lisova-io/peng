import diagnostics.{containsErrors, containsNotes, containsWarnings}

import backend.ir.evaluator.Eval
import backend.irgen.asttranslator.*
import backend.opt.passsetup.OptLevel

import frontend.ast.printAST
import frontend.sema.SemaResult

import overseer.DebugOverseer
import overseer.DefaultOverseer

import sys.process.given
import java.io.PrintWriter
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

enum Command:
  case Help
  case PrintAst(val src: List[String])
  case PrintIr(val src: List[String])
  case Run(val src: List[String])
  case Graph(val src: List[String])

enum Mode:
  case Debug
  case Default

class Options(val cmd: Command)

class Parser[T](val run: Seq[String] => Either[String, (T, Seq[String])]):
  def flatMap[U](f: T => Parser[U]): Parser[U] = Parser(
    run(_).flatMap(r => f(r._1).run(r._2))
  )
  def foreach(f: T => Unit): Parser[Unit]    = map(f)
  def andThen[U](that: Parser[U]): Parser[U] = flatMap(_ => that)
  def map[U](f: T => U): Parser[U]           = Parser(in => run(in).map(r => f(r._1) -> r._2))
  def orElse(that: Parser[T]): Parser[T] = Parser(in =>
    run(in) match
      case Left(_) => that.run(in)
      case right   => right
  )
  def `<|>`(that: Parser[T]): Parser[T] = this.orElse(that)
  def replace[U](x: U): Parser[U]       = this.map(_ => x)
  def many: Parser[List[T]] =
    Parser(in =>
      if in.isEmpty then Right(Nil -> in)
      else
        run(in) match
          case Left(value)        => Right(Nil -> in)
          case Right((res, rest)) => many.run(rest).map(r => (res +: r._1) -> r._2)
    )

object Parser:
  def pure[T](x: T): Parser[T] = Parser(in => Right(x -> in))
  def literal(s: String, expected: String = null): Parser[String] = Parser(in =>
    val exp = if expected == null then s"`$s`" else expected
    in.headOption match
      case None                      => Left(s"expected $exp, got nothing")
      case Some(value) if value == s => Right(value, in.tail)
      case Some(value)               => Left(s"expected $exp, got `$value`")
  )
  def anyString(expected: String = "an argument"): Parser[String] = Parser(in =>
    in.headOption match
      case None        => Left(s"expected $expected, got nothing")
      case Some(value) => Right(value, in.tail)
  )

def parseSource: Parser[String]      = Parser.anyString("a source file")
def parseCommandLiteral(lit: String) = Parser.literal(lit, "a command")

def parsePrintAst: Parser[Command] =
  parseCommandLiteral("ast")
    .andThen(parseSource.many)
    .map(Command.PrintAst(_))

def parsePrintIr: Parser[Command] =
  parseCommandLiteral("ir")
    .andThen(parseSource.many)
    .map(Command.PrintIr(_))

def parseRun: Parser[Command] =
  parseCommandLiteral("run")
    .andThen(parseSource.many)
    .map(Command.Run(_))

def parseGraph: Parser[Command] =
  parseCommandLiteral("graph")
    .andThen(parseSource.many)
    .map(Command.Graph(_))

def parseCmd: Parser[Command] =
  parseCommandLiteral("help").replace(Command.Help)
    <|> parsePrintAst
    <|> parsePrintIr
    <|> parseGraph
    <|> parseRun

def parseOptions: Parser[Options] =
  for {
    cmd <- parseCmd
  } yield Options(cmd)

final private val mode = Mode.Default

object Driver:
  val overseer = mode match
    case Mode.Debug   => DebugOverseer
    case Mode.Default => DefaultOverseer

  private def mapFile[T](file: String, f: String => T): T =
    val source = scala.io.Source.fromFile(file)
    val input =
      try source.mkString.replace("\t", "    ")
      finally source.close()
    f(input)

  private def parse(input: String): Option[List[frontend.ast.Decl]] =
    val lexer                      = overseer.getLexer(input)
    val parser                     = overseer.getParser(lexer)
    val (decls, parserDiagnostics) = parser.parse
    frontend.diagnostics.Diagnostics(input).printDiagnostics(parserDiagnostics)
    if parserDiagnostics.containsErrors then None else Some(decls)

  private def runSema(input: String, decls: List[frontend.ast.Decl]): Option[frontend.ast.AST] =
    val SemaResult(ast, semaDiagnostics) = overseer.getSema.run(decls)
    frontend.diagnostics.Diagnostics(input).printDiagnostics(semaDiagnostics)
    if semaDiagnostics.containsErrors then None else Some(ast)

  private def genIr(ast: frontend.ast.AST): backend.ir.control.Program =
    overseer.getTranslator(ast).gen

  private def parseAndPrintAST(filename: String)(input: String) =
    println(s"$filename:")
    for {
      decls <- parse(input)
      ast   <- runSema(input, decls)
    } printAST(ast)

  private def printIr(filename: String)(input: String) =
    println(s"$filename:")
    for {
      decls <- parse(input)
      ast   <- runSema(input, decls)
      ir = genIr(ast)
    } println(ir)

  private def executeFile(filename: String)(input: String): Unit =
    println(s"$filename:")
    for {
      decls <- parse(input)
      ast   <- runSema(input, decls)
      ir = genIr(ast)
    } println(Eval(ir).eval)

  private def writeToFile(path: String, msg: String): Unit =
    val pw = PrintWriter(File(path))
    try pw.write(msg)
    finally pw.close()

  private def graph(filename: String)(input: String): Unit =
    for {
      decls <- parse(input)
      ast   <- runSema(input, decls)
      ir = genIr(ast)
    } {
      val gv      = backend.graphviz.GraphViz.programToGV(filename, ir)
      val outFile = Paths.get("graphs/" + filename).normalize()
      val outDir  = outFile.getParent();
      Files.createDirectories(outDir)
      writeToFile(outFile.toString + ".dot", gv.toString)
      val svg = ("dot -Tsvg " + outFile.toString + ".dot").!!
      writeToFile(outFile.toString + ".svg", svg)
    }

  private def printHelp =
    println("""peng compiler

available commands:
  help             print this message
  run <FILES...>   execute code from given source files
  ast <FILES...>   print abstract syntax trees for given source files
  ir <FILES...>    generate and print intermediate representation for given source files
  graph <FILES...> generate .svg files with IR CFG for given files""")

  @main def run(args: String*): Unit =
    parseOptions.run(args) match
      case Left(err) =>
        println(err)
        println("for more information see `peng help`")
      case Right((options, _)) =>
        options.cmd match
          case Command.Help          => printHelp
          case Command.Run(src)      => src.foreach(f => mapFile(f, executeFile(f)))
          case Command.PrintAst(src) => src.foreach(f => mapFile(f, parseAndPrintAST(f)))
          case Command.PrintIr(src)  => src.foreach(f => mapFile(f, printIr(f)))
          case Command.Graph(src)    => src.foreach(f => mapFile(f, graph(f)))
