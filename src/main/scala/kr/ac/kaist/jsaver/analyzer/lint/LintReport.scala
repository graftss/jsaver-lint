package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.js.ast.AST

// Data encoding a single instance of a lint rule violation
trait LintReport {
  val rule: LintRule
  val message: String

  override def toString: String = message
}
