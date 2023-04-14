package kr.ac.kaist.jsaver.analyzer.lint.rule

import kr.ac.kaist.jsaver.analyzer.View
import kr.ac.kaist.jsaver.analyzer.lint.LintContext

trait LintRule {
  val name: String
  def validate(ctx: LintContext): Unit
}
