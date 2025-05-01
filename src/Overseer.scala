package overseer

import frontend.ast.AST

import frontend.lex.Lexer
import frontend.lex.Tokens

import frontend.parse.Parser
import frontend.parse.DefaultParser

import frontend.sema.Sema
import frontend.sema.DefaultSema

import backend.irgen.asttranslator.ASTTranslator
import backend.irgen.asttranslator.LoggingTranslator
import backend.irgen.asttranslator.TranslatorCtx
import backend.irgen.asttranslator.DefaultTranslator

import backend.opt.passmanager.PassManager
import backend.opt.passmanager.DefaultManager
import backend.opt.passmanager.LoggingManager
import backend.opt.passsetup.OptLevel
import backend.opt.passsetup.PassSetup

import backend.ir.control.Function
import backend.ir.control.Program

import com.typesafe.scalalogging.StrictLogging

trait Overseer:
  def getTranslator(ast: AST): ASTTranslator
  def getPassManager(program: Program, level: OptLevel): PassManager
  def getLexer(input: String): Lexer
  def getParser(input: Tokens): Parser
  def getSema: Sema

object DebugOverseer extends Overseer with StrictLogging:
  def getLexer(input: String): Lexer =
    val lexer = Lexer(input, debug = true)
    lexer

  def getParser(input: Tokens): Parser =
    val parser = DefaultParser
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
  def getLexer(input: String): Lexer         = Lexer(input, debug = false)
  def getParser(input: Tokens): Parser       = DefaultParser
  def getSema: Sema                          = DefaultSema()
  def getTranslator(ast: AST): ASTTranslator = DefaultTranslator(ast)
  def getPassManager(program: Program, level: OptLevel) =
    PassSetup(DefaultManager(program), level).configure
