package backend.graphviz

import backend.ir.control.*
import backend.ir.ir.*
import backend.ir.irvalue.ImmInt

class GraphVizNode(val node: String):
  var next: List[GraphVizNode] = List()
  def addNode(node: GraphVizNode): Unit =
    next +:= node
  override def toString(): String =
    val printNode = if node(0) == '%' then node.drop(1) else node
    if next.isEmpty then printNode + System.lineSeparator() + "    "
    else next.foldLeft("")((acc: String, nextNode: GraphVizNode) =>
      val printNextNode = if nextNode.node(0) == '%' then nextNode.node.drop(1) else nextNode
      acc + "\"" + printNode + "\"" + " -> " + "\"" + printNextNode + "\"" + System
        .lineSeparator() + "    "
    )

class GraphViz:
  var name: String                      = ""
  var nodes: List[GraphVizNode]         = List()
  def setName(name: String): Unit       = this.name = name
  def addNode(node: GraphVizNode): Unit = nodes :+= node
  override def toString(): String =
    val sep = System.lineSeparator() + "    "
    s"digraph $name {" + sep +
      nodes.mkString
      + System.lineSeparator() + "}"

object GraphViz:
  def programToGV(sourceName: String, program: Program): GraphViz =
    val gv = GraphViz()
    gv.name = sourceName.replace('/', '_').replace('.', '_').replace('-', '_')
    program.fns.foreach((_, fn) => fnToGV(fn, gv))
    gv
  private def fnToGV(fn: Function, gv: GraphViz): GraphViz =
    val blocks = fn.blocks
    blocks.foreach(block =>
      val instr  = block.instrs.last
      val gvnode = GraphVizNode(block.name.name)
      gv.addNode(gvnode)
      instr match
        case Br(cond, tbranch, fbranch) =>
          gvnode.addNode(GraphVizNode(tbranch.name))
          gvnode.addNode(GraphVizNode(fbranch.name))
        case Jmp(label) =>
          gvnode.addNode(GraphVizNode(label.name))
        case Ret(ret) => ()
        case _ =>
          println("last instruction is not terminator for some reason?")
          ???
    )
    gv
