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
import backend.opt.passmanager.Program
import backend.opt.passmanager.LoggingManager

trait Overseer:
  def getTranslator(ast: AST): ASTTranslator
  def getPassManager(program: Program): PassManager

object DebugOverseer extends Overseer:
  def getTranslator(ast: AST)          = LoggingTranslator(ast)
  def getPassManager(program: Program) = LoggingManager(program)

object DefaultOverseer extends Overseer:
  def getTranslator(ast: AST): ASTTranslator = DefaultTranslator(ast)
  def getPassManager(program: Program)       = DefaultManager(program)
