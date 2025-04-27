package driver

import diagnostics.{containsErrors, containsNotes, containsWarnings}

import frontend.ast.printAST
import frontend.sema.SemaResult

import backend.irgen.asttranslator.*
import backend.opt.passsetup.OptLevel

import overseer.DebugOverseer
import overseer.DefaultOverseer
import scala.collection.mutable.HashMap
import backend.ir.evaluator.Eval

enum Command:
  case Help
  case PrintAst(val src: List[String])
  case PrintIr(val src: List[String])
  case Run(val src: List[String])

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
  def literal(s: String): Parser[String] = Parser(in =>
    in.headOption match
      case None                      => Left(s"expected `$s`, got nothing")
      case Some(value) if value == s => Right(value, in.tail)
      case Some(value)               => Left(s"expected `$s`, got `$value`")
  )
  def anyString: Parser[String] = Parser(in =>
    in.headOption match
      case None        => Left(s"expected an argument, got nothing")
      case Some(value) => Right(value, in.tail)
  )

def parseSource: Parser[String] = Parser.anyString

def parsePrintAst: Parser[Command] =
  for {
    _   <- Parser.literal("ast")
    src <- parseSource.many
  } yield Command.PrintAst(src)

def parsePrintIr: Parser[Command] =
  for {
    _   <- Parser.literal("ir")
    src <- parseSource.many
  } yield Command.PrintIr(src)

def parseRun: Parser[Command] =
  for {
    _   <- Parser.literal("run")
    src <- parseSource.many
  } yield Command.Run(src)

def parseCmd: Parser[Command] =
  Parser.literal("help").replace(Command.Help) <|>
    parsePrintAst <|>
    parsePrintIr <|>
    parseRun

def parseOptions: Parser[Options] =
  for {
    cmd <- parseCmd
  } yield Options(cmd)

private val mode = Mode.Default

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

  private def printHelp =
    println("""peng compiler

available commands:
  help             print this message
  run <FILES...>   execute code from given source files
  ast <FILES...>   print abstract syntax trees for given source files
  ir <FILES...>    generate and print intermediate representation for given source files""")

  def run(args: Seq[String]): Unit =
    parseOptions.run(args) match
      case Left(err) => println(err)
      case Right((options, _)) =>
        options.cmd match
          case Command.Help          => printHelp
          case Command.Run(src)      => src.foreach(f => mapFile(f, executeFile(f)))
          case Command.PrintAst(src) => src.foreach(f => mapFile(f, parseAndPrintAST(f)))
          case Command.PrintIr(src)  => src.foreach(f => mapFile(f, printIr(f)))
