package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsBool, AbsObj, AbsState, AbsValue, FlatBot, FlatElem, FlatTop }
import kr.ac.kaist.jsaver.analyzer.lint.LintReport.UNKNOWN
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.{ ASTVal, Bool }
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
    //    println(s"ast kind: ${np.view.jsViewOpt.get.ast.kind}")
    // TODO: if initializer, get rid of the "= " prefix
    s"${indent}${label}: " + np.view.jsViewOpt.map(_.ast).getOrElse(UNKNOWN)
  }

  def readOwnEnvBinding(st: AbsState, env: AbsObj, id: String): AbsValue = {
    (env.getTy.name match {
      case "GlobalEnvironmentRecord" =>
        st(env("DeclarativeRecord").loc).map(readOwnEnvBinding(st, _, id))
      case _ => {
        st(env("SubMap").loc).map(_(id))
      }
    }).getOrElse(AbsValue.Bot)
  }

  def readBindingValue(binding: AbsObj): AbsValue = {
    //    println(s"read binding value of: ${binding}")
    binding.getTy.name match {
      case "ImmutableBinding" | "MutableBinding" => binding("BoundValue")
    }
  }

  def readEnvValue(st: AbsState, env: AbsObj, id: String): AbsValue = {
    // read the value from `env`
    val envBindingRef = readOwnEnvBinding(st, env, id)
    //    println(s"envBinding: ${envBindingRef}, isBottom: ${envBindingRef.absent.isBottom}")
    val envValue = st(envBindingRef.loc).map(readBindingValue).getOrElse(AbsValue.Bot)
    //    println(s"envValue: ${envValue}")

    // if the binding must not be absent, the outer env doesn't need to be considered
    if (envBindingRef.absent.isBottom) {
      envValue
    } else {
      val outerEnvRef = env("OuterEnv").comp.normal.value.loc
      val outerEnvValue = st(outerEnvRef).map(readEnvValue(st, _, id)).getOrElse(AbsValue.Bot)
      envValue ⊔ outerEnvValue
    }
  }

  def jsIdValues(st: AbsState, ast: AST, env: AbsObj): String = {
    ""
  }
}

object LintReport {
  val UNKNOWN = "[unknown]"
}