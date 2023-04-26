package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintError, LintReport, LintSeverity }
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

case class RcInstance(np: NodePoint[Node], st: AbsState, ctorRef: AbsValue, instanceRef: AbsValue, stateRef: AbsValue) {
}

case class NdmsReport() extends LintReport {
  override val rule: LintRule = NoDirectMutationState
  override val severity: LintSeverity = LintError

  override def message: String = "Directly mutated React component state:"
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
            // otherwise, record data about this react component instance.
            val instanceRef = st(CONSTRUCT_NODE_INSTANCE_REF, np).comp.normal.value
            val instance = st(instanceRef.loc).get

            lookupRef(st, instance, "SubMap")
              .flatMap(lookupRef(st, _, "state"))
              .map(_("Value"))
              .map(stateRef => {
                println(s"stateRef: ${stateRef}")
                RcInstance(np, st, ctor, instanceRef, stateRef)
              })
          }
        }
      }

    // for each property write:
    ctx.sem.npMap.filter(isPropRefWritePair).foreach {
      case (np, st) => {
        println(s"loc: ${np.view.jsViewOpt.map(_.ast)}")
        // `V` is a reference record describing the location of the mutated value.
        val Vref = st(Id("V"), np)
        val V = st(Vref.loc).get
        // The `Base` of `V` is the object whose value is mutated.
        val base = V("Base")
        // The `ReferencedName` of `V` is the key associated to the mutated value.
        val referencedName = V("ReferencedName")

        if (rcInstances.exists(rci => locMeet(rci.instanceRef, base))) {
          // if directly mutating an instance of a react component:
          if (!(referencedName ⊓ AbsValue("state")).isBottom) {
            // if the mutated property may be "state":
            //            println("js call string: \n\n" + np.view.jsCallString() + "\n\n\n")

            // TODO: don't report a rule violation if a react component's constructor occurs in the JS call string
            ctx.report(NdmsReport())
          }
        }

        rcInstances.foreach {
          case RcInstance(np, _, ctorRef, instanceRef, stateRef) => {
            val refs = refsInObject(st, stateRef, base).map(_.add(referencedName))
            println(s"refs: $refs")
          }
        }
      }
    }
  }
}
