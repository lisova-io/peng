package overseer

import frontend.ast.AST
import scala.collection.mutable.HashMap
import frontend.lex.Lexer
import frontend.lex.DefaultLexer
import frontend.parse.Parser
import frontend.parse.DefaultParser
import frontend.sema.Sema
import frontend.sema.DefaultSema
import com.typesafe.scalalogging.StrictLogging

trait Overseer:
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

object DefaultOverseer extends Overseer:
  def getLexer(input: String): Lexer  = DefaultLexer(input)
  def getParser(lexer: Lexer): Parser = DefaultParser(lexer)
  def getSema: Sema                   = DefaultSema()
