package frontend.errorprint

import frontend.ast
import frontend.lex.{Span, SpanCmp}
import frontend.parse.ParseError

def getPairs[T](items: Seq[T]): Seq[(T, T)] = {
  items match
    case x :: (next @ (y :: tail)) => (x, y) +: getPairs(next)
    case _                         => Seq()
}

extension (s: String)
  def splitIntoLines: Seq[(Int, Int)] =
    val lfs = (s.zipWithIndex.filter((c, _) => c == '\n')).map((_, i) => i)
    getPairs(0 +: lfs.toList :+ s.length)

def fmtChar(c: Char): String =
  c match
    case '\n' => "\\n"
    case c    => c.toString

def printErrs(input: String, errs: Seq[ParseError]): Unit = {
  val lines = input.splitIntoLines.zipWithIndex
  for (err <- errs.sortBy(_.span)(SpanCmp)) {
    val ((b, e), line) =
      lines.find((l, _) => l._1 <= err.span.b && err.span.e <= l._2).get
    val offset        = err.span.b - b
    val offsetLen     = offset.toString.length
    val front         = "      | "
    val frontWithLine = " " * (5 - offsetLen) + (line + 1).toString + " | "
    val c             = input(err.span.b)
    println(err.msg)
    println(frontWithLine + input.slice(b + 1, e - 1))
    println(front + " " * offset + '^')
  }
}
