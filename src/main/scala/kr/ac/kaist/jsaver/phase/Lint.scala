package kr.ac.kaist.jsaver.phase

import kr.ac.kaist.jsaver.{ JSAVERConfig, js }
import kr.ac.kaist.jsaver.analyzer.AbsSemantics
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, MayCallees }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintWalker }
import kr.ac.kaist.jsaver.analyzer.lint.rule.{ ArrayCallbackReturn, LintRule }
import kr.ac.kaist.jsaver.cfg.{ Branch, Call, Exit, InstNode, Linear }
import kr.ac.kaist.jsaver.ir.{ ArrowInst, CallInst, CondInst, ILet, ISeq, Id, NormalInst }
import kr.ac.kaist.jsaver.js.ASTWalker
import kr.ac.kaist.jsaver.js
import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.util.OptionKind

case class LintResult()

case object Lint extends Phase[AbsSemantics, LintConfig, LintResult] {
  override val name: String = "lint"
  override val help: String = "check semantic lint rules against static analysis result"

  val walker = new LintWalker()

  val rules: List[LintRule] = List(ArrayCallbackReturn)

  def apply(
    sem: AbsSemantics,
    jsaverConfig: JSAVERConfig,
    config: LintConfig = defaultConfig
  ): LintResult = {
    val ctx = new LintContext(sem)

    // read the exit state from the analysis result
    val exitState = sem.getState(sem.runJobsRp)

    rules.foreach(_.validate(ctx))
    ctx.logReports()

    LintResult()
  }

  override def defaultConfig: LintConfig = LintConfig()

  override val options: List[(String, OptionKind[LintConfig], String)] = List()

  def callsToString(st: AbsState): Unit = {
    st.lint.mayCall.foreach(elt => {
      val (ctx, MayCallees(callees)) = elt
      callees.foreach(callee => {
        val callerName = walker.funcDefs.get(ctx.hash).flatMap(_.name).getOrElse("?")
        val calleeName = walker.funcDefs.get(callee).flatMap(_.name).getOrElse("?")
        println(s"- ${callerName} may call ${calleeName}: ${ctx.view.toCallStackString}")
      })
    })
  }
}

case class LintConfig() extends Config