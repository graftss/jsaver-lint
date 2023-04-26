package kr.ac.kaist.jsaver.analyzer.lint.rule

import kr.ac.kaist.jsaver.analyzer.domain.{ AAst, AbsLoc, AbsObj, AbsState, AbsValue, BasicObj, FlatElem }
import kr.ac.kaist.jsaver.analyzer.{ NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id
import kr.ac.kaist.jsaver.js.ast.ClassDeclaration0

case class ObjPath(keys: Iterable[AbsValue] = List()) {
  def add(key: AbsValue): ObjPath = ObjPath(keys ++ List(key))
}

case class ClassEval(np: NodePoint[Node], st: AbsState, loc: AbsLoc, obj: AbsObj) {
  def protoObj(): Option[AbsObj] = {
    st(obj(AbsValue("Prototype")).loc)
  }

  def className(): Option[String] = {
    val ast = obj(AbsValue("ECMAScriptCode")).ast

    val declAst = ast.getSingle match {
      case FlatElem(AAst(ast)) => ast.findKindAbove("ClassDeclaration")
      case _ => None
    }

    declAst.flatMap {
      case ClassDeclaration0(id, _, _, _) => Some(id.toString)
      case _ => None
    }
  }

  override def toString: String = s"ClassEval:\n  ${np}\n  $loc\n  $obj"
}

trait LintRule {
  val name: String
  def validate(ctx: LintContext): Unit

  // Returns `true` if the abstract values `a` and `b` have abstract locations with a nontrivial meet.
  def locMeet(a: AbsValue, b: AbsValue): Boolean =
    !(a.loc ⊓ b.loc).isBottom

  def refsInObject(st: AbsState, objRef: AbsValue, targetRef: AbsValue, path: ObjPath = ObjPath(List())): List[ObjPath] = {
    // First check if the target reference may meet the original object reference.
    val rootResult = if (locMeet(objRef, targetRef)) {
      List(path)
    } else {
      List()
    }
    //    println("refs call:")
    //    println(s"  path=${path}, rootResult=$rootResult")
    //    println(s"  objRef:$objRef, targetRef:$targetRef")
    //    println(s"  \n\nobj:${st(objRef.loc)}\n\n")
    //    println(s"  \n\ntarget:${st(targetRef.loc)}\n\n")

    val deepResults = st(objRef.loc)
      .flatMap(obj => st(obj("SubMap").loc))
      .map {
        case elem: BasicObj.PropMapElem => {
          elem.map.flatMap {
            case (key, value) => {
              // for each key-value pair in the object, recursively check the value for `targetRef`:
              val nextPath = path.add(AbsValue(key))
              println(s"  nextpath: ${nextPath}, meet:${}")
              val result = st(value.loc).map(_("Value"))
                .map(refsInObject(st, _, targetRef, nextPath))
                .getOrElse(List())
              result
            }
          }
        }
        // TODO: if merged object, return data showing that the result is imprecise
        case _ => List[ObjPath]()
      }.getOrElse(List())

    println(s"path: ${path}, root:${rootResult}, deep:${deepResults}")

    rootResult ++ deepResults
  }

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
    case (np, st) => {
      val loc = getClassObjLoc(pair)
      st(loc).map(ClassEval(np, st, loc, _))
    }
  }

  // Returns `true` if the abstract object `obj` may have the object located at `targetProtoLoc` *anywhere* along its
  // prototype chain, not including `obj` itself.
  def classMayHaveProto(st: AbsState, obj: AbsObj, targetProtoLoc: AbsLoc): Boolean = {
    val PROTOTYPE_KEY = AbsValue("Prototype")
    val thisProtoLoc = obj(PROTOTYPE_KEY).loc

    // if the object's direct prototype and the target prototype have a nontrivial meet, they may match
    if (!(thisProtoLoc ⊓ targetProtoLoc).isBottom) {
      true
    } else {
      st(thisProtoLoc) match {
        case Some(protoObj) => classMayHaveProto(st, protoObj, targetProtoLoc)
        case None => false
      }
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

  def lookupDataPropComp(obj: AbsObj): AbsValue =
    obj(AbsValue("Value")).comp.normal.value
}
