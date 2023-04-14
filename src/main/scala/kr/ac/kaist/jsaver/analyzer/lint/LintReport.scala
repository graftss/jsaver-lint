package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.View
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule
import kr.ac.kaist.jsaver.ir.ASTVal
import kr.ac.kaist.jsaver.js.ast.AST

// Data encoding a single instance of a lint rule violation
trait LintReport {
  val rule: LintRule
  def message: String

  override def toString: String = message

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
}
