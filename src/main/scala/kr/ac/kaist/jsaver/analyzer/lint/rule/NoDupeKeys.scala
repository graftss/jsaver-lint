package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.{ NodePoint, View }
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsObj, AbsState, AbsValue }
import kr.ac.kaist.jsaver.analyzer.lint.{ LintContext, LintReport }
import kr.ac.kaist.jsaver.cfg.{ Branch, Exit, InstNode, Linear, Node }
import kr.ac.kaist.jsaver.ir.{ ASTVal, Addr, Clo, Const, Cont, Func, IAccess, Id, SimpleValue }
import kr.ac.kaist.jsaver.js.ast.{ ObjectLiteral, ObjectLiteral0, ObjectLiteral1, ObjectLiteral2 }

import scala.collection.mutable.ListBuffer

case class NdkReport(view: View, ast: ObjectLiteral, property: AbsValue, oldValue: AbsValue, newValue: AbsValue) extends LintReport {
  override val rule: LintRule = NoDupeKeys

  override def message: String = {
    val lines = ListBuffer(
      "Defined duplicate key in object literal:",
      s"  object literal: ${ast}",
      s"  property: ${property}",
      s"  old value: ${oldValue}",
      s"  new value: ${newValue}",
    )

    jsCallString(view).foreach(s => lines += s"  source: ${s}")

    lines.mkString("\n")
  }
}

object NoDupeKeys extends LintRule {
  override val name: String = "no-dupe-keys"

  private val INSTRUMENTED_UID = 24505
  private val PROPERTY_ID = Id("P")
  private val OLD_DESC_ID = Id("current")
  private val NEW_DESC_ID = Id("Desc")
  private val DESC_VALUE_KEY = AbsValue("Value")

  // Find all valid nodepoints in `OrdinaryDefineOwnProperty` at the instruction:
  //  2:app __x2__ = (ValidateAndApplyPropertyDescriptor O P extensible Desc current)
  def isInstrumentedInstruction(pair: (NodePoint[Node], AbsState)): Boolean =
    pair._1.node.getInst.exists(_.uid == INSTRUMENTED_UID)

  def validatePropDefn(pair: (NodePoint[Node], AbsState)): Option[NdkReport] = {
    val (np, st) = pair

    // Check that the topmost AST is an `ObjectLiteral` node.
    val astOpt: Option[ObjectLiteral] = np.view.jsViewOpt match {
      case Some(jsView) => {
        jsView.ast.exprChild match {
          case Some(ast @ ObjectLiteral0(_, _)) => Some(ast)
          case Some(ast @ ObjectLiteral1(_, _, _)) => Some(ast)
          case Some(ast @ ObjectLiteral2(_, _, _)) => Some(ast)
          case _ => None
        }
      }
      case None => None
    }

    // Then check the value of the old property descriptor:
    astOpt.flatMap(ast => {
      println(s"view: ${np.view.jsViewOpt}")

      val oldDescValue = st(OLD_DESC_ID, np)

      // If it has a nonempty location, the property may already exist, so we have a dupe key.
      if (!oldDescValue.loc.isBottom) {
        val newDescValue = st(NEW_DESC_ID, np)
        (st(oldDescValue.loc), st(newDescValue.loc)) match {
          case (Some(oldDesc), Some(newDesc)) => {
            val oldValue = oldDesc(DESC_VALUE_KEY)
            val newValue = newDesc(DESC_VALUE_KEY)
            Some(NdkReport(np.view, ast, st(PROPERTY_ID, np), oldValue, newValue))
          }
          case _ => None
        }
      } else {
        None
      }
    })
  }

  override def validate(ctx: LintContext): Unit = {
    ctx.sem.npMap.filter(isInstrumentedInstruction)
      .map(validatePropDefn)
      .foreach {
        case Some(report) => ctx.report(report)
        case _ => ()
      }
  }
}
