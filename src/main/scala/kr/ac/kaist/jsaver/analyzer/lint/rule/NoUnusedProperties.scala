package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsObj, AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.LintContext
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.Id

object NoUnusedProperties extends LintRule {
  override val name: String = "no-unused-properties"

  private val INSTRUMENTED_INST_UID = 24505
  private val PROPERTY_ID = Id("P")
  private val OLD_DESC_ID = Id("current")
  private val NEW_DESC_ID = Id("Desc")
  private val DESC_VALUE_KEY = AbsValue("Value")

  // Find all valid nodepoints in `OrdinaryDefineOwnProperty` at the instruction:
  //  2:app __x2__ = (ValidateAndApplyPropertyDescriptor O P extensible Desc current)
  def isInstrumentedInstruction(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.getInst.exists(_.uid == INSTRUMENTED_INST_UID)

  def isObjLiteralDefn(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.view.jsViewOpt match {
      case Some(jsView) if jsView.ast.exprChild.exists(_.kind == "ObjectLiteral") => true
      case _ => false
    }

  def isInstrumentedGetValueInst(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.uid == 19556

  override def validate(ctx: LintContext): Unit = {
    // index all defined properties of object literals
    val objProps = ctx.sem.npMap.filter(isInstrumentedInstruction)
      .filter(isObjLiteralDefn)
      .map {
        case (np, st) => {
          val O = st(Id("O"), np)
          val P = st(PROPERTY_ID, np)
          new DefinedObjProp(np, O, P)
        }
      }

    val objPropIndex = new DefinedObjPropIndex(objProps)

    ctx.sem.npMap.filter(isInstrumentedGetValueInst)
      .foreach(pair => {
        println("GetValue call:")
        val (np, st) = pair
        val Vref = st(Id("V"), np)
        val V = st(Vref.loc).get
        val base = V("Base")
        val refName = V("ReferencedName")

        objPropIndex.findMatches(base, refName)
          .foreach(_.setAccessed)
      })

    objPropIndex.unaccessed.foreach(dop => {
      println(dop)
    })
  }

  class DefinedObjProp(val np: NodePoint[Node], val ref: AbsValue, val prop: AbsValue) {
    val id: Int = DefinedObjProp.nextId
    var accessed: Boolean = false

    def setAccessed: Unit = {
      accessed = true
    }

    override def toString: String = {
      s"DefinedObjProp(${ref}, ${prop})"
    }
  }

  object DefinedObjProp {
    private var nextId: Int = 0
    def getId: Int = {
      val result = nextId
      nextId += 1
      result
    }
  }

  class DefinedObjPropIndex(elts: Iterable[DefinedObjProp]) {
    var table: Map[AbsValue, List[DefinedObjProp]] = Map()

    indexElts

    def indexElts(): Unit = {
      elts.foreach(dop => {
        val refList = table.get(dop.ref) match {
          case Some(oldRefList) => oldRefList
          case None => List()
        }
        table += dop.ref -> (dop :: refList)
      })
    }

    def findMatches(base: AbsValue, referencedName: AbsValue): Iterable[DefinedObjProp] = {
      table.filter(pair => locMeet(base, pair._1))
        .flatMap(pair => {
          pair._2.filter(dop => !(dop.prop ⊓ referencedName).isBottom)
        })
    }

    def unaccessed: Iterable[DefinedObjProp] =
      table.foldLeft(List[DefinedObjProp]())((x, kv) => x ++ kv._2.filter(!_.accessed))
  }
}
