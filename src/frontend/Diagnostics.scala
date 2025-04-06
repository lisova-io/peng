package frontend.diagnostics

import frontend.lex.{Lexer, Offset, Span, SpanOrdering, Token, WithSpan}

object Color:
  private def ansi(code: Any): String = 0x1b.toChar + s"[${code}m"
  val Reset                           = ansi(0)
  val Bold                            = ansi(1)
  val Red                             = ansi(31)
  val Yellow                          = ansi(33)
  val Green                           = ansi(32)

enum Severity extends Ordered[Severity]:
  case Error
  case Warning
  case Note

  private def toInt: Int =
    this match
      case Error   => 2
      case Warning => 1
      case Note    => 0

  override def toString(): String =
    this match
      case Error   => Color.Bold + Color.Red + "error" + Color.Reset
      case Warning => Color.Bold + Color.Yellow + "warning" + Color.Reset
      case Note    => Color.Bold + Color.Green + "note" + Color.Reset

  override def compare(that: Severity): Int = this.toInt - that.toInt

implicit object SeverityOrdering extends Ordering[Severity]:
  override def compare(x: Severity, y: Severity): Int = x.compare(y)

class Message(val span: Span, val msg: String)
object Message:
  def unapply(m: Message): (Span, String) = m.span -> m.msg

class Diagnostic(val severity: Severity, val messages: List[Message]):
  def this(severity: Severity, span: Span, msg: String) = this(severity, Message(span, msg) :: Nil)
  def unapply: (Severity, List[Message]) = (severity, messages)
  def firstMsg: Message                  = messages.head

object Diagnostic:
  def error(span: Span, msg: String) = Diagnostic(Severity.Error, span, msg)
  def error(msgs: List[Message])     = Diagnostic(Severity.Error, msgs)
  // def warning(span: Span, msg: String) = Diagnostic(Severity.Warning, span, msg)
  // def note(span: Span, msg: String)    = Diagnostic(Severity.Note, span, msg)

extension (diags: List[Diagnostic])
  def maxSeverity: Option[Severity] =
    if diags.isEmpty then None else Some(diags.map(_.severity).max)

  def containsErrors: Boolean   = diags.find(_.severity == Severity.Error).isDefined
  def containsWarnings: Boolean = diags.find(_.severity == Severity.Warning).isDefined
  def containsNotes: Boolean    = diags.find(_.severity == Severity.Note).isDefined

private def getPairs[T](items: Seq[T]): Seq[(T, T)] = {
  items match
    case x +: (next @ (y +: tail)) => (x, y) +: getPairs(next)
    case _                         => Seq()
}

extension (s: String)
  def splitIntoLines: Seq[(Int, Int)] =
    val lfs = (s.zipWithIndex.filter((c, _) => c == '\n')).map((_, i) => i)
    getPairs(0 +: lfs.toList :+ s.length).map(l => if l._1 != 0 then (l._1 + 1, l._2) else l)

def fmtChar(c: Char): String =
  c match
    case '\n' => "\\n"
    case c    => c.toString

def printDiagnostics(input: String, diagnostics: Seq[Diagnostic]): Unit = {
  if diagnostics.isEmpty then return

  var firstErr = true

  val lines = input.splitIntoLines.zipWithIndex
  for d <- diagnostics.sortBy(d => (d.firstMsg.span, d.severity)) do
    if !firstErr then println()
    firstErr = false
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
