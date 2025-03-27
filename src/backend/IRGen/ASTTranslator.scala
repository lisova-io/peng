package backend.irgen.asttranslator

import frontend.ast._
import backend.ir._
import backend.control._
import backend.irvalue._

trait ASTTranslator:
  def genFn(fn: FnDecl): Function

trait TranslatorCtx:
  def genRegName: String

class TCtx extends TranslatorCtx:
  var current: Int = 0
  def genRegName: String =
    val ret = current.toString
    current += 1
    ret

class IRValueGen(ctx: TranslatorCtx):
  def createIntVar: Var =
    Var(ctx.genRegName, VType.i32)
  def createNeg(operand: Value): Neg =
    Neg(createIntVar, operand)
  def createAdd(lhs: Value, rhs: Value): Add =
    Add(createIntVar, lhs, rhs)
  def createSub(lhs: Value, rhs: Value): Sub =
    Sub(createIntVar, lhs, rhs)

class IRGen(ctx: TranslatorCtx):
  def genUnary(unary: UnaryExpr) =
    def matchUnary =
      unary.op.value match
        // case UnaryOp.Minus => Neg(Var(ctx.genRegName, VType.i32), )
        case _ => ???
    ???

def genNLExpr(nl: NumLitExpr): Value =
  ImmInt(nl.value.value)
