package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.domain.AbsState
import kr.ac.kaist.jsaver.analyzer.lint.LintReport.UNKNOWN
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.ASTVal
import kr.ac.kaist.jsaver.js.ast.{ AST, Expression }

// Data encoding a single instance of a lint rule violation
trait LintReport {
  val rule: LintRule
  val severity: LintSeverity
  def message: String

  override def toString: String = message

  def callStringStr(np: NodePoint[Node], label: String = "call string", indent: String = "  "): String =
    s"${indent}${label}: " + np.view.jsCallString().getOrElse(UNKNOWN)

  def viewAstStr(np: NodePoint[Node], label: String = "source", indent: String = "  "): String = {
    println(s"ast kind: ${np.view.jsViewOpt.get.ast.kind}")
    s"${indent}${label}: " + np.view.jsViewOpt.map(_.ast).getOrElse(UNKNOWN)
  }

  def jsIdValues(st: AbsState, ast: AST): String = {
    ""
  }
}

object LintReport {
  val UNKNOWN = "[unknown]"
}