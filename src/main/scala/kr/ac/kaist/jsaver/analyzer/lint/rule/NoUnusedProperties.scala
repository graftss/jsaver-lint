package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.NodePoint
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsObj, AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.rule.NoUnusedProperties.DefinedObjProp
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintError, LintReport, LintSeverity, LintWarning }
import kr.ac.kaist.jsaver.cfg.Node
import kr.ac.kaist.jsaver.ir.{ ERef, EStr, IAccess, IApp, Id, RefId }
import kr.ac.kaist.jsaver.js.ast.AST

case class NupReport(dop: DefinedObjProp) extends LintReport {
  override val rule: LintRule = NoUnusedProperties
  override val severity: LintSeverity = LintWarning

  // TODO
  override val astNodes = Nil
  override val nodePoints = Nil

  override def message: String = {
    List(
      "Unused property of object literal:",
      s"  object literal: ${dop.objAst}",
      s"  property: ${dop.propDefAst}",
    ).mkString("\n")
  }
}

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
          val P = st(PROPERTY_ID, np).exclam
          new DefinedObjProp(np, O, P)
        }
      }

    val objPropIndex = new DefinedObjPropIndex(objProps)

    ctx.sem.npMap.filter(isInstrumentedGetValueInst)
      .foreach(pair => {
        val (np, st) = pair
        val Vref = st(Id("V"), np)
        val V = st(Vref.loc).get
        val base = V("Base")
        val refName = V("ReferencedName")

        objPropIndex.findMatches(base, refName)
          .foreach(_.setAccessed)
      })

    objPropIndex.unaccessedAst.foreach(dop => ctx.report(NupReport(dop)))
  }

  class DefinedObjProp(val np: NodePoint[Node], val ref: AbsValue, val prop: AbsValue) {
    val id: Int = DefinedObjProp.nextId
    var accessed: Boolean = false

    // The AST of the object containing the property definition
    def objAst: AST = np.view.jsViewOpt.get.ast

    // The AST of the property definition (a single key-value pair being defined on an object literal)
    def propDefAst: AST = np.view.calls.find(cv => {
      cv.astOpt.exists(ast => ast.kind == "PropertyDefinition" || ast.kind == "PropertyDefinitionList")
    }).get.astOpt.get

    def setAccessed: Unit = { accessed = true }

    override def toString: String = {
      val ast = np.view.jsViewOpt.map(_.ast.toString).getOrElse("???")
      s"DefinedObjProp(${prop}, $ast, $propDefAst})"
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

  class DefinedObjPropIndex(val elts: Iterable[DefinedObjProp]) {
    var index: Map[AST, List[DefinedObjProp]] = Map()

    indexElts()

    def indexElts(): Unit = {
      elts.foreach(dop => {
        index = index.updatedWith(dop.propDefAst)({
          case Some(value) => Some(dop :: value)
          case None => Some(List(dop))
        })
      })
    }

    def findMatches(base: AbsValue, referencedName: AbsValue): Iterable[DefinedObjProp] = {
      elts.filter(dop => locMeet(base, dop.ref) && !(dop.prop ⊓ referencedName).isBottom)
    }

    def unaccessedAst: Iterable[DefinedObjProp] = {
      index.flatMap {
        case (_, dops) => {
          if (dops.exists(_.accessed)) None
          else dops.headOption
        }
      }
    }
  }
}
