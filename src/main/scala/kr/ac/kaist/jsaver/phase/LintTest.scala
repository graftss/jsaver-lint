package kr.ac.kaist.jsaver.phase

import kr.ac.kaist.jsaver.{ JSAVERConfig, js }
import kr.ac.kaist.jsaver.analyzer.AbsSemantics
import kr.ac.kaist.jsaver.analyzer.lint.LintWalker
import kr.ac.kaist.jsaver.js.ASTWalker
import kr.ac.kaist.jsaver.js
import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.util.OptionKind

case class LintTestResult()

case object LintTest extends Phase[Script, LintConfig, LintTestResult] {
  override val name: String = "linttest"
  override val help: String = "check semantic lint rules against static analysis result"

  val walker = new LintWalker()

  def apply(
    script: Script,
    jsaverConfig: JSAVERConfig,
    config: LintConfig = defaultConfig
  ): LintTestResult = {

    walker.walk(script)

    println(script.toTreeString())

    println("func defs: " + walker.funcDefs)
    println("mutations: " + walker.mutations)

    LintTestResult()
  }

  override def defaultConfig: LintConfig = LintConfig()

  override val options: List[(String, OptionKind[LintConfig], String)] = List()
}