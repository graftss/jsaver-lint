package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

case class RcInstance(classEval: ClassEval, instanceRef: AbsValue, stateRef: AbsValue)

object NoDirectMutationState extends LintRule {
  override val name: String = "no-direct-mutation-state"

  // Instrument the following instruction of `Construct` to detect class instance constructions
  // 4: Normal[6516] return [? __x2__]
  private val CONSTRUCT_NODE_ID = 6516
  private val CONSTRUCT_NODE_INSTANCE_REF = Id("__x2__")
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

    rcClassEvals.foreach {
      case ce @ ClassEval(np, st, loc, obj) => {
        println(s"class eval: ${obj(AbsValue("SourceText"))}")
        val hasProto = classMayHaveProto(st, obj, rcLoc)
        println(s"hasProto: ${hasProto}")
      }
    }

    return

    def meetsRcObjs(value: AbsValue): Iterable[ClassEval] =
      rcClassEvals.filter {
        case ClassEval(np, st, loc, obj) => !(loc ⊓ value.loc).isBottom
      }

    ctx.sem.npMap.filter(isConstructPair)
      .flatMap {
        case (np, st) => {
          println(s"construct np: ${np}")
          val F = st(Id("F"), np)
          println(s"F: ${F}")
          println(s"meets: ${}")
          val instanceRef = st(CONSTRUCT_NODE_INSTANCE_REF, np).comp.normal.value
          val instance = st(instanceRef.loc).get
          val subMapRef = instance("SubMap")
          val subMap = st(subMapRef.loc).get
          meetsRcObjs(F).foreach(ce => println(s"met ce: ${ce}"))
          println(s"instance: ${instance}")
          println(s"submap: ${subMap}")
          val stateDpRef = subMap("state")
          val stateDp = st(stateDpRef.loc).get
          val stateRef = stateDp("Value")
          val state = st(stateRef.loc).get
          val stateSubMapRef = state("SubMap")
          val stateSubMap = st(stateSubMapRef.loc).get
          println(s"stateRef: ${stateRef}")
          println(s"state: ${state}")
          println(s"statesubmap: ${stateSubMap}")
          None
        }
      }

    ctx.sem.npMap.filter(isPropRefWritePair)
      .foreach {
        case (np, st) => {
          println("putvalue: ")
          val Vref = st(Id("V"), np)
          val W = st(Id("W"), np)
          val VrefRec = st(Vref.loc).get
          val Vbase = VrefRec("Base")
          println(s"  Vref: ${Vref}")
          println(s"  VrefRec: ${VrefRec}")
          println(s"  Vbase: ${Vbase}")
          println(s"  W: ${W}")
        }
      }
  }
}
