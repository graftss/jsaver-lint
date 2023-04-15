package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.View
import kr.ac.kaist.jsaver.analyzer.domain.AbsState
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.ir.ASTVal
import kr.ac.kaist.jsaver.js.ast.{ AST, Expression }

// Data encoding a single instance of a lint rule violation
trait LintReport {
  val rule: LintRule
  val severity: LintSeverity
  def message: String

  override def toString: String = message

  // Map a `View` to its JS function call string
  def jsCallString(view: View): Option[String] = {
    view.jsViewOpt.map(jsView => {
      val callString = jsView.calls.flatMap {
        case ast if ast.kind == "Initializer" => ast.children.head match {
          case ASTVal(ast) => Some(ast)
          case _ => None
        }
        case ast => Some(ast)
      }.reverse.mkString(", ")

      s"call string: ${callString}"
    })
  }

  def jsIdValues(st: AbsState, ast: AST): String = {
    ""
  }
}
