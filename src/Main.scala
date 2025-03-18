import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation._

import frontend.ast
import backend.ir

def getPairs[T](items: Seq[T]): Seq[(T, T)] = {
  items match
    case x :: (next @ (y :: tail)) => (x, y) +: getPairs(next)
    case _                         => Seq()
}

extension (s: String)
  def splitIntoLines: Seq[(Int, Int)] =
    val lfs = (s.zipWithIndex.filter((c, _) => c == '\n')).map((_, i) => i)
    getPairs(0 +: lfs.toList :+ s.length)

def fmtExpectation(e: Expectation): String = e match
  case OneOfStr(_, strs)       => s"one of ${strs.mkString(", ")}"
  case InRange(_, b, e)        => s"[$b-$e]"
  case StartOfString(_)        => "start of string"
  case EndOfString(_, length)  => "end of string"
  case Length(_, exp, act)     => s"length $exp, got $act"
  case ExpectedFailureAt(_, m) => "fail"
  case Fail(_)                 => "fail"
  case FailWith(_, msg)        => s"fail with $msg"
  case WithContext(ctx, inner) => s"$ctx: ${fmtExpectation(inner)}"

def fmtChar(c: Char): String =
  c match
    case '\n' => "\\n"
    case c    => c.toString

def printErrs(input: String, errs: Seq[Parser.Error]): Unit = {
  errs.sortBy(_.failedAtOffset)
  val lines = input.splitIntoLines.zipWithIndex
  for (err <- errs) {
    val failOffset = err.failedAtOffset
    val ((b, e), line) =
      lines.find((l, _) => l._1 <= failOffset && failOffset <= l._2).get
    val offset        = failOffset - b
    val offsetLen     = offset.toString.length
    val front         = "      | "
    val frontWithLine = " " * (5 - offsetLen) + (line + 1).toString + " | "
    val c             = input(err.failedAtOffset)
    val expectations  = err.expected.map(fmtExpectation).toList.mkString(" | ")
    println(s"expected $expectations")
    println(frontWithLine + input.slice(b + 1, e - 1))
    println(front + " " * offset + '^')
  }
}

@main def main() =
  val a = ir.Instr()
