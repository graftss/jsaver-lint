package kr.ac.kaist.jsaver.phase

import kr.ac.kaist.jsaver.JSAVERConfig
import kr.ac.kaist.jsaver.analyzer.AbsSemantics
import kr.ac.kaist.jsaver.util.OptionKind

case class LintResult()

case object Lint extends Phase[AbsSemantics, LintConfig, LintResult] {
  override val name: String = "lint"
  override val help: String = "check semantic lint rules against static analysis result"

  def apply(
    in: AbsSemantics,
    jsaverConfig: JSAVERConfig,
    config: LintConfig = defaultConfig
  ): LintResult = {
    LintResult()
  }

  override def defaultConfig: LintConfig = LintConfig()

  override val options: List[(String, OptionKind[LintConfig], String)] = List()
}

case class LintConfig() extends Config