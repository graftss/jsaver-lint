package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.domain.BasicObj.PropMap
import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.domain.{ ASimple, AbsBool, AbsComp, AbsObj, AbsState, AbsValue, BasicObj, FlatBot, FlatElem, FlatTop }
import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableStmt.{ DisableStmtAll, DisableStmtRules }
import kr.ac.kaist.jsaver.analyzer.lint.LintReport.UNKNOWN
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.{ ASTVal, Bool, Id, Str }
import kr.ac.kaist.jsaver.js.ast.{ AST, Expression }

// Data encoding a single instance of a lint rule violation
trait LintReport {
  def rule: LintRule
  def severity: LintSeverity
  def message: String

  /**
   * A list of AST nodes associated with the rule violation, if any.
   * The elements of the list are optional for convenience, since JS AST nodes are optional
   * fields of control points, from which these AST nodes are generally inherited.
   */
  def astNodes: List[Option[AST]]

  /** A list of control points associated with the rule violation, if any. */
  def nodePoints: List[NodePoint[Node]]

  /**
   * A report is automatically disabled if at least one of its associated AST nodes has a comment
   *  which disables the report's rule.
   */
  def astDisabled: Boolean = astNodes.exists(_.exists(_.stmtLintComments.exists(_.isRuleAstDisabled(rule))))

  def npDisabled: Boolean =
    // for some associated control point:
    nodePoints.exists(
      // for some JS call in the control point's view:
      _.view.jsCalls.exists(
        // for each lint comment on the call's statement:
        _.ast.stmtLintComments.exists(
          // check if the lint comment eval-disables this report's rule
          _.isRuleEvalDisabled(rule)
        )
      )
    )

  def disabled: Boolean = astDisabled || npDisabled

  override def toString: String = message

  def callStringStr(np: NodePoint[Node], indent: Int = 1, label: String = "call string"): String =
    s"${spaces(indent)}${label}: " + np.view.jsCallString().getOrElse(UNKNOWN)

  def viewAstStr(np: NodePoint[Node], indent: Int = 1, label: String = "source"): String = {
    //    println(s"ast kind: ${np.view.jsViewOpt.get.ast.kind}")
    // TODO: if initializer, get rid of the "= " prefix
    s"${spaces(indent)}${label}: " + np.view.jsViewOpt.map(_.ast).getOrElse(UNKNOWN)
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
    binding.getTy.name match {
      case "ImmutableBinding" | "MutableBinding" => binding("BoundValue").exclam
      case _ => {
        println(s"unknown binding type: ${binding.getTy.name}")
        AbsValue.Bot
      }
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

  def spaces(indent: Int): String = "  " * indent

  def propMapStr(st: AbsState, propMap: PropMap, indent: Int): String = {
    val itemSpace = spaces(indent)
    propMap.map {
      case (k, dpRef) => {
        st(dpRef.loc).map(dp => {
          val value = dp("Value").exclam
          val valueStr = k match {
            case ASimple(Str("prototype")) | ASimple(Str("constructor")) => value.toString
            case _ => {
              val value = dp("Value").exclam
              jsValueStr(st, value, indent)
            }
          }

          s"${itemSpace}${k} -> ${valueStr}"
        }).getOrElse("???")
      }
    }.mkString("{\n", "\n", s"\n${spaces(indent - 1)}}")
  }

  def jsValueStr(st: AbsState, value: AbsValue, indent: Int): String = {
    // primitive javascript values
    val simpleStr = if (!value.simple.isBottom) {
      value.simple.toString
    } else {
      ""
    }

    // objects
    val objStr = if (!value.loc.isBottom) {
      st(value.loc).flatMap(obj => {
        obj.getTy.name match {
          case _ => st(obj("SubMap").loc).map {
            case submap: BasicObj.OrderedMap => propMapStr(st, submap.map, indent + 1)
            case submap: BasicObj.KeyWiseMap => propMapStr(st, submap.map, indent + 1)
            case submap @ _ => {
              println(s"hello [${submap.getClass}]: ${submap}")
              ""
            }
          }
        }
      }).getOrElse("")
    } else {
      ""
    }

    simpleStr + objStr
  }

  def jsIdValuesStr(st: AbsState, ast: AST, env: AbsObj, indent: Int): String = {
    val idNames = ast.childIds.map(id => id.toString)
    val outer = spaces(indent)

    idNames.map(id => s"${outer}${id} -> ${jsValueStr(st, readEnvValue(st, env, id), indent)}")
      .mkString("\n")
  }

  // Compute the lexical environment of the execution context at index `stackIdx` from the top of the
  // execution stack.
  def execContextEnv(np: NodePoint[Node], st: AbsState, stackIdx: Int): Option[AbsObj] = {
    val stackRef = st(Id("EXECUTION_STACK"), np)

    st(stackRef.loc).flatMap {
      case elem: BasicObj.KeyWiseList if elem.values.length >= stackIdx + 1 => {
        val ctxRef = elem.values(elem.values.length - 1 - stackIdx)
        val ctx = st(ctxRef.loc).get

        st(ctx("LexicalEnvironment").comp.normal.value.loc)
      }
      case _ => None
    }
  }
}

object LintReport {
  val UNKNOWN = "[unknown]"

  val LOG: Boolean = true
}