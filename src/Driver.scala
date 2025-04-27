package driver

import diagnostics.{containsErrors, containsNotes, containsWarnings}

import frontend.ast.printAST
import frontend.sema.SemaResult

import backend.irgen.asttranslator.*
import backend.opt.passsetup.OptLevel

import overseer.DebugOverseer
import overseer.DefaultOverseer
import scala.collection.mutable.HashMap

enum Command:
  case PrintAst(val src: List[String])
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

def parseRun: Parser[Command] =
  for {
    _   <- Parser.literal("run")
    src <- parseSource.many
  } yield Command.Run(src)

def parseCmd: Parser[Command] = parsePrintAst <|> parseRun

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
      try source.mkString
      finally source.close()
    f(input)

  private def parseAndPrintAST(filename: String)(input: String): Unit =
    println(s"$filename:")
    val lexer                      = overseer.getLexer(input)
    val parser                     = overseer.getParser(lexer)
    val (decls, parserDiagnostics) = parser.parse
    frontend.diagnostics.Diagnostics(input).printDiagnostics(parserDiagnostics)
    if parserDiagnostics.containsErrors then return
    val SemaResult(ast, semaDiagnostics) = overseer.getSema.run(decls)
    frontend.diagnostics.Diagnostics(input).printDiagnostics(semaDiagnostics)
    if semaDiagnostics.containsErrors then return
    printAST(ast)

  private def executeFile(input: String): Unit =
    val lexer                      = overseer.getLexer(input)
    val parser                     = overseer.getParser(lexer)
    val (decls, parserDiagnostics) = parser.parse
    frontend.diagnostics.Diagnostics(input).printDiagnostics(parserDiagnostics)
    if parserDiagnostics.containsErrors then return
    val SemaResult(ast, semaDiagnostics) = overseer.getSema.run(decls)
    frontend.diagnostics.Diagnostics(input).printDiagnostics(semaDiagnostics)
    if semaDiagnostics.containsErrors then return
    val translator = overseer.getTranslator(ast)
    val ir         = translator.gen
    // TODO
    // ir.foreach((_, actual) => println(actual))
    // val passmanager = overseer.getPassManager(ir, optLevel)
    // val newIR       = passmanager.addPass(TrivialDCE()).perform
    // newIR.foreach((_, actual) => println(actual))
    // val g = parseArgs(args)
    // execute(g)

  def run(args: Seq[String]): Unit =
    parseOptions.run(args) match
      case Left(err) => println(err)
      case Right((options, _)) =>
        options.cmd match
          case Command.Run(src)      => src.foreach(f => mapFile(f, executeFile))
          case Command.PrintAst(src) => src.foreach(f => mapFile(f, parseAndPrintAST(f)))
end Driver
