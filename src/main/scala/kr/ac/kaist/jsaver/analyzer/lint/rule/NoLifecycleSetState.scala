package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.{ JSCallToken, JSView, NodePoint }
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsLoc, AbsObj, AbsState, AbsValue, FlatElem }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

case class ClassEval(np: NodePoint[Node], st: AbsState, obj: AbsObj) {
  def getProto(): Option[AbsObj] = {
    st(obj(AbsValue("Prototype")).loc)
  }
}

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

object NoLifecycleSetState extends LintRule {
  override val name = "no-lifecycle-set-state"

  private val REACT_COMPONENT_CLASSNAME = "Component"
  private val REACT_LIFECYCLE_METHODS = List("componentDidMount", "componentWillUpdate", "componentDidUpdate")

  // Instrument the following instruction of `ClassDeclaration[0,0].BindingClassDeclarationEvaluation`
  //   5:return value
  private val CLASS_DECL_EVAL_NODE_ID = 5675
  private val CLASSNAME_ID = Id("className")
  private val CLASSOBJ_ID = Id("value")
  private val PROTOTYPE_KEY = AbsValue("Prototype")

  def isClassEvalPair(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.uid == CLASS_DECL_EVAL_NODE_ID

  def findReactComponentClass(pair: (NodePoint[Node], AbsState)): Boolean = {
    val (np, st) = pair

    // read the class name from an class declaration, looking for the designated name of the
    // placeholder React component class
    st(CLASSNAME_ID, np).comp.normal.value.simple.str.getSingle match {
      case FlatElem(name) if name.str == REACT_COMPONENT_CLASSNAME => true
      case _ => false
    }
  }

  def getClassObjLoc(pair: (NodePoint[Node], AbsState)): AbsLoc = pair match {
    case (np, st) => st(CLASSOBJ_ID, np).loc
  }

  // Bundle a class object with its pair data into a `ClassEval`
  def pairToClassEval(pair: (NodePoint[Node], AbsState)): Option[ClassEval] = pair match {
    case (np, st) => st(getClassObjLoc(pair)).map(ClassEval(np, st, _))
  }

  def classMayHaveProto(classEval: ClassEval, protoLoc: AbsLoc): Boolean = classEval match {
    case ClassEval(np, st, obj) =>
      val thisProtoLoc = obj(PROTOTYPE_KEY).loc
      if (!(thisProtoLoc ⊓ protoLoc).isBottom) {
        // if the class eval's prototype and the target prototype have a nontrivial meet, they may match
        true
      } else {
        false
      }
  }

  def lookupRef(st: AbsState, obj: AbsObj, key: String): Option[AbsObj] =
    lookupRef(st, obj, AbsValue(key))

  def lookupRef(st: AbsState, obj: AbsObj, key: AbsValue): Option[AbsObj] =
    st(obj(key).loc)

  def lookupRefPath(st: AbsState, obj: AbsObj, path: List[String]): Option[AbsObj] =
    path.foldLeft[Option[AbsObj]](Some(obj))((obj, key) => obj.flatMap(lookupRef(st, _, AbsValue(key))))

  // Finds the `SubMap` with properties corresponding to the JS prototype of `obj`
  def lookupJsProto(st: AbsState, obj: AbsObj): Option[AbsObj] =
    lookupRefPath(st, obj, List("SubMap", "prototype", "Value", "SubMap"))

  def lookupDataProp(obj: AbsObj): AbsValue =
    obj(AbsValue("Value")).comp.normal.value

  override def validate(ctx: LintContext): Unit = {
    val classEvalPairs = ctx.sem.npMap.filter(isClassEvalPair)
    val rcLocOpt = classEvalPairs.find(findReactComponentClass)
      .map(getClassObjLoc)

    // If no designated react component has been defined, skip validating this rule.
    if (rcLocOpt.isEmpty) return
    val rcLoc = rcLocOpt.get

    // iterate over all class evaluations
    val rcMethodsList = classEvalPairs.flatMap(pairToClassEval)
      // TODO: check that the class has react component as a prototype
      .flatMap {
        case ce @ ClassEval(_, st, obj) => {
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
                  println(s"call token ast: ${ast}")
                  rcMethodsList.zip(rcMethodsList.map(_.findMatch(fnRef))).foreach {
                    case (rcm, matches) if !matches.isEmpty => {
                      // found a rule violation: the `setState` method of the react component class may be called
                      // while a react component lifecycle method is on the call stack.
                      println(s"matches: ${matches}")
                    }
                    case _ => ()
                  }
                }
              }
            }
          }
          println("called setState:\n" + np.view.toJsCallsString)
        }
      }
    }
  }
}
