package kr.ac.kaist.jsaver.util

import scala.util.matching.Regex

case class Span(
  start: Pos = Pos(),
  end: Pos = Pos(),
  rawPreComment: Option[String] = None
) {
  // validity check
  def valid: Boolean = start.valid && end.valid

  def preComment: Option[String] = {
    val commentPattern = "(?:(?://)|(?:/\\*))\\s+(.*)".r
    rawPreComment.flatMap(commentPattern.findFirstMatchIn)
      .map(_.group(1))
  }

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
