package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, JSView, NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.domain.{ AAst, AbsLoc, AbsObj, AbsState, AbsValue, FlatBot, FlatElem, FlatTop }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintError, LintReport, LintSeverity }
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id
import kr.ac.kaist.jsaver.js.ast.{ AST, ClassDeclaration0 }

import scala.collection.mutable.ListBuffer

case class RcMethods(classEval: ClassEval, methodRefs: Map[String, AbsValue]) {
  // Returns the list of all method names whose references may meet `fnRef`
  def findMatch(fnRef: AbsValue): List[String] = {
    methodRefs.foldLeft(List[String]()) {
      case (result, (methodName, methodRef)) => {
        fnRef ⊓ methodRef match {
          case AbsValue.Bot => result
          case _ => methodName :: result
        }
      }
    }
  }
}

trait Key[T] {
  def lookup(st: AbsState, obj: AbsObj): Option[T]
}

case class RefKey(key: AbsValue) extends Key[AbsObj] {
  def lookup(st: AbsState, obj: AbsObj): Option[AbsObj] = {
    st(obj(key).loc)
  }
}

case class NlssReport(methodName: String, className: Option[String], view: View) extends LintReport {
  override val rule: LintRule = NoLifecycleSetState
  override val severity: LintSeverity = LintError

  override def message: String = {
    val lines = ListBuffer(
      "Called `setState` from a lifecycle method:",
      s"  component name: ${className.getOrElse("[anonymous]")}",
      s"  method name: ${methodName}",
    )

    view.jsCallString.foreach(s => lines += s"  source: ${s}")

    lines.mkString("\n")
  }
}

object NoLifecycleSetState extends LintRule {
  override val name = "no-lifecycle-set-state"

  private val REACT_LIFECYCLE_METHODS = List("componentDidMount", "componentWillUpdate", "componentDidUpdate")

  override def validate(ctx: LintContext): Unit = {
    val classEvalPairs = ctx.sem.npMap.filter(isClassEvalPair)
    val rcLocOpt = classEvalPairs.find(findReactComponentClass)
      .map(getClassObjLoc)

    // If no designated react component has been defined, skip validating this rule.
    if (rcLocOpt.isEmpty) return
    val rcLoc = rcLocOpt.get

    // iterate over all class evaluations
    val rcMethodsList = classEvalPairs.flatMap(pairToClassEval)
      // filter class evaluations to just those with the react component class as a prototype
      .filter {
        case ClassEval(np, st, loc, obj) => classMayHaveProto(st, obj, rcLoc)
      }
      .flatMap {
        case ce @ ClassEval(_, st, _, obj) => {
          lookupJsProto(st, obj).map(jsProto => {
            val methodRefs = REACT_LIFECYCLE_METHODS
              .foldLeft(Map[String, AbsValue]()) {
                case (map, methodName) => lookupRef(st, jsProto, methodName) match {
                  case Some(methodObj) => map + (methodName -> lookupDataProp(methodObj))
                  case None => map
                }
              }
            RcMethods(ce, methodRefs)
          })
        }
      }

    // for each called JS function:
    ctx.sem.npMap.filter { case (np, _) => np.node.uid == 4068 }.foreach {
      case (np, st) => {
        val rcss = lookupJsProto(st, st(rcLoc).get)
          .flatMap(lookupRef(st, _, "setState"))
          .map(lookupDataProp)

        val calleeLoc = st(Id("F"), np).loc
        val calleeMayBeRcss = !(calleeLoc ⊓ rcss.get.loc).isBottom
        // if the react component `setState` method may be called:
        if (calleeMayBeRcss) {
          np.view.jsViewOpt.foreach {
            case JSView(_, calls, _) => {
              calls.foreach {
                case JSCallToken(ast, fnRef) => {
                  rcMethodsList.zip(rcMethodsList.map(_.findMatch(fnRef))).foreach {
                    case (rcm, matches) => matches.foreach(methodName =>
                      ctx.report(NlssReport(methodName, rcm.classEval.className(), np.view)))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
