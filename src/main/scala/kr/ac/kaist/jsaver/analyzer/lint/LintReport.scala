package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, View }
import kr.ac.kaist.jsaver.analyzer.domain.AbsState
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.ir.ASTVal
import kr.ac.kaist.jsaver.js.ast.{ AST, Expression }

// Data encoding a single instance of a lint rule violation
trait LintReport {
  val rule: LintRule
  val severity: LintSeverity
  def message: String

  override def toString: String = message

  def jsIdValues(st: AbsState, ast: AST): String = {
    ""
  }
}
