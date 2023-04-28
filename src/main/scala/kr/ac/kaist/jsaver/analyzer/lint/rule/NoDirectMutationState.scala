package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintError, LintReport, LintSeverity }
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id
import kr.ac.kaist.jsaver.js.ast.AST

import scala.collection.mutable.ListBuffer

trait NdmsReport extends LintReport {
  override val rule: LintRule = NoDirectMutationState
  override val severity: LintSeverity = LintError

  def message(np: NodePoint[Node], header: String, footer: Option[List[String]] = None): String = {
    val lines = ListBuffer(
      header,
      viewAstStr(np),
      callStringStr(np),
    )

    footer.foreach(footerLines => lines ++= footerLines)

    lines.mkString("\n")
  }
}

case class OverwriteStateReport(np: NodePoint[Node]) extends NdmsReport {
  override def message(): String = {
    super.message(np, "Overwrote `state` property of React component:")
  }
}

case class MutateStateReport(np: NodePoint[Node], statePath: ObjPath) extends NdmsReport {
  override val rule: LintRule = NoDirectMutationState
  override val severity: LintSeverity = LintError

  override def message(): String = {
    super.message(np, "Mutated React component state:", Some(List(
      s"  mutated state path: ${statePath}"
    )))
  }
}

object NoDirectMutationState extends LintRule {
  override val name: String = "no-direct-mutation-state"

  // Instrument the following instruction of `Construct` to detect class instance constructions
  // 4: Normal[6516] return [? __x2__]
  private val CONSTRUCT_NODE_ID = 6516
  private val CONSTRUCT_NODE_INSTANCE_REF = Id("__x2__")
  private val CONSTRUCT_NODE_CTOR_OBJ = Id("F")
  def isConstructPair(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.uid == CONSTRUCT_NODE_ID

  // Instrument the following instruction of `PutValue` to detect writes to property references:
  //   9:let succeeded = [? __x6__]
  private val PUTVALUE_PROP_REF_NODE_ID = 25281
  def isPropRefWritePair(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.uid == PUTVALUE_PROP_REF_NODE_ID

  override def validate(ctx: LintContext): Unit = {
    val classEvalPairs = ctx.sem.npMap.filter(isClassEvalPair)
    val rcLocOpt = classEvalPairs.find(findReactComponentClass)
      .map(getClassObjLoc)

    // If no designated react component has been defined, skip validating this rule.
    if (rcLocOpt.isEmpty) return
    val rcLoc = rcLocOpt.get

    val rcClassEvals = classEvalPairs.flatMap(pairToClassEval)
      .filter {
        case ClassEval(_, st, _, obj) => classMayHaveProto(st, obj, rcLoc)
      }

    // returns an iterator over react components `ClassEval` objects which may meet `value`.
    def meetsRcObjs(value: AbsValue): Iterable[ClassEval] =
      rcClassEvals.filter {
        case ClassEval(np, st, loc, obj) => !(loc ⊓ value.loc).isBottom
      }

    val rcInstances = ctx.sem.npMap.filter(isConstructPair)
      .flatMap {
        case (np, st) => {
          // iterate over all constructor calls, looking for constructions of react components:
          val ctor = st(CONSTRUCT_NODE_CTOR_OBJ, np)

          if (meetsRcObjs(ctor).isEmpty) {
            // if `ctor` cannot refer to a react component, skip this object instance.
            None
          } else {
            val instanceRef = st(CONSTRUCT_NODE_INSTANCE_REF, np).comp.normal.value
            Some(RcInstance(np, st, ctor, instanceRef))
          }
        }
      }

    // for each property write:
    ctx.sem.npMap.filter(isPropRefWritePair).foreach {
      case (npWrite, stWrite) => {
        // `V` is a reference record describing the location of the mutated value.
        val Vref = stWrite(Id("V"), npWrite)
        val V = stWrite(Vref.loc).get
        // The `Base` of `V` is the object whose value is mutated.
        val base = V("Base")
        // The `ReferencedName` of `V` is the key associated to the mutated value.
        val referencedName = V("ReferencedName")

        // Check if the property write is overwriting a react component's `state` field:
        rcInstances.filter(rci => {
          // If the reference record's base may be the react component instance itself,
          locMeet(rci.instanceRef, base) &&
            // if the reference record's referenced name may be `state`,
            !(referencedName ⊓ AbsValue("state")).isBottom &&
            // and if the react component's constructor isn't on the call stack.
            !rci.ctorInCallString(npWrite)
        }).foreach(rci => ctx.report(OverwriteStateReport(npWrite)))

        // Check if the property write is overwriting a value within a react component's `state` object:
        rcInstances.foreach(rci => {
          // If the react component's constructor isn't on the call stack,
          if (!rci.ctorInCallString(npWrite)) {
            rci.stateRef(stWrite).foreach(stateRef => {
              // for each intersection between the reference record's base
              // and locations referenced by the component's `state` object:
              refsInObject(stWrite, stateRef, base).map(_.add(referencedName)).foreach(objPath => {
                ctx.report(MutateStateReport(npWrite, objPath))
              })
            })
          }
        })
      }
    }
  }

  case class RcInstance(np: NodePoint[Node], stCtor: AbsState, ctorRef: AbsValue, instanceRef: AbsValue) {
    // Returns `true` if the view of `np` has this instance's constructor in its call string.
    def ctorInCallString(np: NodePoint[Node]): Boolean = {
      np.view.jsViewOpt.exists(jsView => {
        jsView.calls.exists(callToken => {
          !(callToken.value.loc ⊓ ctorRef.loc).isBottom
        })
      })
    }

    // Compute the value of the react component instance's `state` field in the state `st`
    def stateRef(st: AbsState): Option[AbsValue] = {
      val instance = st(instanceRef.loc).get
      lookupRef(st, instance, "SubMap")
        .flatMap(lookupRef(st, _, "state"))
        .map(_("Value"))
    }
  }
}
