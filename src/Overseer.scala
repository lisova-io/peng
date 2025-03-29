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

trait Overseer:
  def getTranslator(ast: AST): ASTTranslator
  def getPassManager(program: HashMap[String, Function]): PassManager

object DebugOverseer extends Overseer:
  def getTranslator(ast: AST)                            = LoggingTranslator(ast)
  def getPassManager(program: HashMap[String, Function]) = DefaultManager(program)

object DefaultOverseer extends Overseer:
  def getTranslator(ast: AST): ASTTranslator             = DefaultTranslator(ast)
  def getPassManager(program: HashMap[String, Function]) = DefaultManager(program)
