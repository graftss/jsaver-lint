package kr.ac.kaist.jsaver.analyzer.lint.rule

import kr.ac.kaist.jsaver.analyzer.domain.{ AbsLoc, AbsObj, AbsState, AbsValue, FlatElem }
import kr.ac.kaist.jsaver.analyzer.{ NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

trait LintRule {
  val name: String
  def validate(ctx: LintContext): Unit

  def findReactComponentClass(pair: (NodePoint[Node], AbsState)): Boolean = {
    val CLASSNAME_ID = Id("className")
    val REACT_COMPONENT_CLASSNAME = "Component"

    val (np, st) = pair

    // read the class name from a class declaration, looking for the designated name of the
    // placeholder React component class
    st(CLASSNAME_ID, np).comp.normal.value.simple.str.getSingle match {
      case FlatElem(name) if name.str == REACT_COMPONENT_CLASSNAME => true
      case _ => false
    }
  }

  // Instrument the following instruction of `ClassDeclaration[0,0].BindingClassDeclarationEvaluation`
  //   5:return value
  private val CLASS_DECL_EVAL_NODE_ID = 5675

  // Filters state pairs for the above class eval instruction.
  def isClassEvalPair(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.uid == CLASS_DECL_EVAL_NODE_ID

  // Returns the location of a class object, given a `pair` at the
  def getClassObjLoc(pair: (NodePoint[Node], AbsState)): AbsLoc = {
    val CLASSOBJ_ID = Id("value")
    val (np, st) = pair

    st(CLASSOBJ_ID, np).loc
  }

  // Bundle a class object with its pair data into a `ClassEval`
  def pairToClassEval(pair: (NodePoint[Node], AbsState)): Option[ClassEval] = pair match {
    case (np, st) => st(getClassObjLoc(pair)).map(ClassEval(np, st, _))
  }

  // Returns `true` if the
  def classMayHaveProto(classEval: ClassEval, protoLoc: AbsLoc): Boolean = classEval match {
    case ClassEval(_, _, obj) =>
      val PROTOTYPE_KEY = AbsValue("Prototype")
      val thisProtoLoc = obj(PROTOTYPE_KEY).loc

      // if the class eval's prototype and the target prototype have a nontrivial meet, they may match
      !(thisProtoLoc ⊓ protoLoc).isBottom
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
}
