package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.lint.LintContext

object NoDirectMutationState extends LintRule {
  override val name: String = "no-direct-mutation-state"

  override def validate(ctx: LintContext): Unit = {

  }
}
