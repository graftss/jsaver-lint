package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsLoc, AbsState, AbsValue, FlatElem }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

object NoLifecycleSetState extends LintRule {
  override val name = "no-lifecycle-set-state"

  private val REACT_COMPONENT_CLASSNAME = "Component"

  // Instrument the following instruction of `ClassDeclaration[0,0].BindingClassDeclarationEvaluation`
  //   2:value.SourceText = (get-syntax ClassDeclaration)
  private val CLASS_DECL_EVAL_INST_ID = 5982
  private val CLASSNAME_ID = Id("className")
  private val CLASSOBJ_ID = Id("value")

  def isClassEvalInstruction(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.getInst.exists(_.uid == CLASS_DECL_EVAL_INST_ID)

  def findReactComponentClass(pair: (NodePoint[Node], AbsState)): Boolean = {
    val (np, st) = pair

    // read the class name from an class declaration, looking for the designated name of the
    // placeholder React component class
    st(CLASSNAME_ID, np).comp.normal.value.simple.str.getSingle match {
      case FlatElem(name) if name.str == REACT_COMPONENT_CLASSNAME => true
      case _ => false
    }
  }

  def getClassObjLoc(pair: (NodePoint[Node], AbsState)): AbsLoc = {
    val (np, st) = pair
    st(CLASSOBJ_ID, np).loc
  }

  def hasPrototype(pair: (NodePoint[Node], AbsState), protoLoc: AbsLoc): Boolean = {
    false
  }

  override def validate(ctx: LintContext): Unit = {
    val classEvals = ctx.sem.npMap.filter(isClassEvalInstruction)
    val locOpt = classEvals.find(findReactComponentClass)
      .map(getClassObjLoc)

    if (locOpt.isEmpty) return

    val reactComponentLoc = locOpt.get

    println(s"react comopnoent loc: ${reactComponentLoc}")

    classEvals.filter {
      case (np, st) => {
        st(getClassObjLoc((np, st))) match {
          case Some(classObj) => {
            val protoLoc = classObj(AbsValue("Prototype")).loc
            println(s"proto: ${protoLoc}")
            println(s"match : ${protoLoc.equals(reactComponentLoc)}")
          }
          case None =>
        }
        false
      }
    }

  }
}
