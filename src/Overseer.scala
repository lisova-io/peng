package overseer

import backend.irgen.asttranslator.ASTTranslator
import backend.opt.passmanager.PassManager
import backend.irgen.asttranslator.LoggingTranslator
import backend.irgen.asttranslator.TranslatorCtx
import frontend.ast.AST
import backend.opt.passmanager.DefaultManager
import scala.collection.mutable.HashMap
import backend.ir.control.Function
import backend.irgen.asttranslator.DefaultTranslator
import backend.opt.passmanager.LoggingManager
import backend.opt.passsetup.OptLevel
import backend.opt.passsetup.PassSetup
import frontend.lex.Lexer
import frontend.lex.DefaultLexer
import frontend.parse.Parser
import frontend.parse.DefaultParser
import frontend.sema.Sema
import frontend.sema.DefaultSema
import com.typesafe.scalalogging.StrictLogging
import backend.ir.control.Program

trait Overseer:
  def getTranslator(ast: AST): ASTTranslator
  def getPassManager(program: Program, level: OptLevel): PassManager
  def getLexer(input: String): Lexer
  def getParser(lexer: Lexer): Parser
  def getSema: Sema

object DebugOverseer extends Overseer with StrictLogging:
  def getLexer(input: String): Lexer =
    val lexer = DefaultLexer(input)
    logger.warn(s"Using DEFAULT class ${lexer.getClass().getName()} in debug mode")
    lexer

  def getParser(lexer: Lexer): Parser =
    val parser = DefaultParser(lexer)
    logger.warn(s"Using DEFAULT class ${parser.getClass().getName()} in debug mode")
    parser

  def getSema: Sema =
    val sema = DefaultSema()
    logger.warn(s"Using DEFAULT class ${sema.getClass().getName()} in debug mode")
    sema

  def getTranslator(ast: AST) = LoggingTranslator(ast)

  def getPassManager(program: Program, level: OptLevel) =
    PassSetup(LoggingManager(program), level).configure

object DefaultOverseer extends Overseer:
  def getLexer(input: String): Lexer         = DefaultLexer(input)
  def getParser(lexer: Lexer): Parser        = DefaultParser(lexer)
  def getSema: Sema                          = DefaultSema()
  def getTranslator(ast: AST): ASTTranslator = DefaultTranslator(ast)
  def getPassManager(program: Program, level: OptLevel) =
    PassSetup(DefaultManager(program), level).configure
