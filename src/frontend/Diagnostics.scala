package frontend.diagnostics

import frontend.lex.Span
import frontend.lex.given
import diagnostics.Diagnostic as BaseDiagnostic
import diagnostics.DiagnosticsPrinter as BaseDiagnosticsPrinter
import diagnostics.Severity
import diagnostics.given

class Message(val span: Span, val msg: String)
object Message:
  def unapply(m: Message): (Span, String) = m.span -> m.msg

class Diagnostic(severity: Severity, val messages: List[Message]) extends BaseDiagnostic(severity):
  def this(severity: Severity, span: Span, msg: String) = this(severity, Message(span, msg) :: Nil)
  def unapply: (Severity, List[Message]) = (severity, messages)
  def firstMsg: Message                  = messages.head

object Diagnostic:
  def error(span: Span, msg: String) = Diagnostic(Severity.Error, span, msg)
  def error(msgs: List[Message])     = Diagnostic(Severity.Error, msgs)
// def warning(span: Span, msg: String) = Diagnostic(Severity.Warning, span, msg)
// def note(span: Span, msg: String)    = Diagnostic(Severity.Note, span, msg)

given BaseDiagnosticsPrinter[Diagnostic, String] with
  type Context = String
  override def run(input: String, diagnostics: Seq[Diagnostic]): Unit = {
    if diagnostics.isEmpty then return

    var firstDiagnostic = true
    val lines           = input.splitIntoLines.zipWithIndex
    for d <- diagnostics.sortBy(d => (d.firstMsg.span, d.severity)) do
      if !firstDiagnostic then println()
      firstDiagnostic = false
      print(s"${d.severity}: ")
      for Message(span, msg) <- d.messages do
        println(msg)
        val linesToPrint = lines.filter((l, _) => l._1 <= span.e && l._2 >= span.b)
        for line <- linesToPrint do
          val ((b, e), i)   = line
          val lineLen       = i.toString.length
          val frontWithLine = " " * (5 - lineLen) + (i + 1).toString + " | "
          println(frontWithLine + input.slice(b, e))
          print("      | ")
          print(" " * (span.b - b) + '^')
          if span.len > 1 then print("~" * (span.len - 1))
          println()
  }

private def getPairs[T](items: Seq[T]): Seq[(T, T)] =
  items match
    case x +: (next @ (y +: tail)) => (x, y) +: getPairs(next)
    case _                         => Seq()

extension (s: String)
  def splitIntoLines: Seq[(Int, Int)] =
    val lfs = (s.zipWithIndex.filter((c, _) => c == '\n')).map((_, i) => i)
    getPairs(0 +: lfs.toList :+ s.length).map(l => if l._1 != 0 then (l._1 + 1, l._2) else l)

private def fmtChar(c: Char): String =
  c match
    case '\n' => "\\n"
    case c    => c.toString
