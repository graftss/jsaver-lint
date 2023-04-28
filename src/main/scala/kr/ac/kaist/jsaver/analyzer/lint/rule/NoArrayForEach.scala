package kr.ac.kaist.jsaver.analyzer.lint.rule

import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, AbsValue, FlatBot, FlatElem, FlatTop }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintImprecision, LintReport, LintSeverity, LintWarning }
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id
import kr.ac.kaist.jsaver.js.ast.AST
import kr.ac.kaist.jsaver.spec.algorithm.Algo

abstract class NafeReport() extends LintReport {
  override val rule: LintRule = NoArrayForEach
}

case class PreciseNafeReport(np: NodePoint[Node]) extends NafeReport {
  override val severity: LintSeverity = LintWarning

  override def message: String = {
    List(
      "Called `Array.prototype.forEach` method:",
      viewAstStr(np),
      callStringStr(np),
    ).mkString("\n")
  }
}

case class ImpreciseNafeReport() extends NafeReport {
  override def message: String = "imprecise report"

  override val severity: LintSeverity = LintImprecision
}

object NoArrayForEach extends LintRule {
  override val name = "no-array-for-each"

  // Instrumented instruction of `BuiltinFunctionObject.Call.algo`:
  //   9:app result = (F.Code thisArgument argumentsList undefined)
  val INSTRUMENTED_INST_UID = 4175
  val FOREACH_ALGO_NAME = "GLOBAL.Array.prototype.forEach"

  def isInstrumentedInstruction(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.getInst.exists(_.uid == INSTRUMENTED_INST_UID)

  def validateFuncCall(pair: (NodePoint[Node], AbsState)): Option[NafeReport] = {
    val (np, st) = pair
    val f = st(st(Id("F"), np).loc).get

    val astOpt = np.view.jsViewOpt.map(_.ast)

    f(AbsValue("Code")).func.getSingle match {
      case FlatElem(afunc) if afunc.algo.name == FOREACH_ALGO_NAME => {
        val x = st(st.lookupGlobal(Id("CONTEXT")).loc)

        Some(PreciseNafeReport(np))
      }
      case FlatTop => Some(ImpreciseNafeReport())
      case _ => None
    }
  }

  override def validate(ctx: LintContext): Unit = {
    ctx.sem.npMap.filter(isInstrumentedInstruction)
      .flatMap(validateFuncCall)
      .foreach(ctx.report)
  }
}
