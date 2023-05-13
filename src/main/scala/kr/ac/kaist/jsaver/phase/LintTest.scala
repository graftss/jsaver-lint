package kr.ac.kaist.jsaver.phase

import kr.ac.kaist.jsaver.{JSAVERConfig, js}
import kr.ac.kaist.jsaver.analyzer.AbsSemantics
import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableNext.{DisableNextAll, DisableNextRules}
import kr.ac.kaist.jsaver.analyzer.lint.comment.LintComment
import kr.ac.kaist.jsaver.analyzer.lint.comment.NamedDecl.NamedClassDecl
import kr.ac.kaist.jsaver.analyzer.lint.{LintUtil, LintWalker, ParserTest}
import kr.ac.kaist.jsaver.js.{ASTWalker, Parser}
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

    println(script.toTreeString(collapseExpr = true))

    LintTestResult()
  }

  def testComment(testDesc: String, commentBody: String, expected: LintComment, ast: Option[AST] = None): Unit = {
    println(s"result: ${LintComment.parse(commentBody, ast)}")
    LintComment.parse(commentBody, ast) match {
      case Some(observed) if observed == expected => println("wootles")
      case Some(lc) => println(s"Parsed incorrect lint comment: ${lc}.")
      case _ => println(s"Failed to parse lint comment: ${commentBody}")
    }
  }

  override def defaultConfig: LintConfig = LintConfig()

  override val options: List[(String, OptionKind[LintConfig], String)] = List()
}