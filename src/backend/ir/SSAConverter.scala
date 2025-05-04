package backend.ir.ssaconverter

import backend.ir.dominators.*
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.util.boundary, boundary.break

trait SSAConverter[Var, Label, Block] extends Dominators[Label]:
  def vertexes: HashMap[Label, Block]
  def vars: HashMap[Var, Set[Label]]
  def insertPhi(block: Block, variable: Var, from: Label): Unit
  def getPhis(block: Block, variable: Var): Option[Set[Label]]
  def ssaFn: Unit =
    val stack: Stack[Label] = Stack()
    val df                  = dominationFrontier
    val sdom                = strictDominators
    for (v, defs) <- vars do
      for labelDefs <- defs do stack.push(labelDefs)
      while !stack.isEmpty do
        val defLabel = stack.pop

        if df contains defLabel then
          for domfLabel <- df(defLabel) do
            boundary:
              val domfBlock = vertexes(domfLabel)
              val opt       = getPhis(domfBlock, v)
              opt match
                case Some(labels) =>
                  if labels.contains(defLabel) then break()
                case None => ()
              insertPhi(domfBlock, v, defLabel)
              stack.push(domfLabel)
