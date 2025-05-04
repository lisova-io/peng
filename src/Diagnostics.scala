package diagnostics

import frontend.lex.{Lexer, Offset, Span, Token, WithSpan}
import scala.io.AnsiColor as Color

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
      case Error   => Color.BOLD + Color.RED + "error" + Color.RESET
      case Warning => Color.BOLD + Color.YELLOW + "warning" + Color.RESET
      case Note    => Color.BOLD + Color.BLUE + "note" + Color.RESET

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
