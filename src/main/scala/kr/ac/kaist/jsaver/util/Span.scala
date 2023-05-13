package kr.ac.kaist.jsaver.util

import kr.ac.kaist.jsaver.analyzer.lint.comment.LintComment
import kr.ac.kaist.jsaver.js.ast.Lexical

import scala.util.matching.Regex
import scala.util.parsing.input.Position

case class Span(
  var start: Pos = Pos(),
  var end: Pos = Pos(),
  var rawPreComment: Option[List[Lexical]] = None
) {
  // validity check
  def valid: Boolean = start.valid && end.valid

  // conversion to string
  override def toString: String = toString(useIndex = false)
  def toString(useIndex: Boolean): String = {
    val Pos(sl, sc, si) = start
    val Pos(el, ec, ei) = end
    if (!valid) "?"
    else if (useIndex) s"$si-$ei"
    else if (sl == el) s"$sl:$sc-$ec"
    else s"$start-$end"
  }
}

object Span {
  def apply(start: Position, end: Position): Span = {
    println(s"start: ${start.line}:${start.column}, end:${end.line}:${end.column}")
    Span(Pos(start.line, start.column), Pos(end.line, end.column))
  }
}
