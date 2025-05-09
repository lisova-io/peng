package diagnostics

import frontend.lex.{Lexer, Offset, Span, Token, WithSpan}

object Color:
  private def ansi(code: Any): String = 0x1b.toChar + s"[${code}m"
  val Reset                           = ansi(0)
  val Bold                            = ansi(1)
  val Red                             = ansi(31)
  val Yellow                          = ansi(33)
  val Green                           = ansi(32)
  val Blue                            = ansi(34)

enum Severity extends Ordered[Severity] {
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
      case Note    => Color.Bold + Color.Blue + "note" + Color.Reset

  override def compare(that: Severity): Int = this.toInt - that.toInt
}

given Ordering[Severity] with
  def compare(x: Severity, y: Severity): Int = x.compare(y)

trait Diagnostic(val severity: Severity)

trait Diagnostics[D <: Diagnostic]:
  def printDiagnostics(diags: Seq[D]): Unit

extension (diags: Seq[Diagnostic]) {
  def containsErrors: Boolean   = diags.find(_.severity == Severity.Error).isDefined
  def containsWarnings: Boolean = diags.find(_.severity == Severity.Warning).isDefined
  def containsNotes: Boolean    = diags.find(_.severity == Severity.Note).isDefined
}
