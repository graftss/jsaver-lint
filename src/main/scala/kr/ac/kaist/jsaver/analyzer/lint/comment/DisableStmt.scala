package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.js.ast.AST

/** Disables one or more lint rules for the statement following the comment. */
object DisableStmt extends LintCommentImpl {
  type Elem = DisableStmtElem
  val key: String = "lint-disable-stmt"

  trait DisableStmtElem extends LintComment {
    override def disablesRule(ruleName: String): Boolean = this match {
      case DisableStmtAll => true
      case DisableStmtRules(ruleNames) => ruleNames.contains(ruleName)
    }
  }

  // Indicates that all rules should be disabled for the decorated AST node.
  case object DisableStmtAll extends DisableStmtElem

  // Indicates that only rules whose name is contained in `ruleNames`
  // should be disabled for the decorated AST node.
  case class DisableStmtRules(ruleNames: List[String]) extends DisableStmtElem

  override def parse(commentBody: String, words: List[String], ast: Option[AST]): Option[DisableStmtElem] = {
    words.length match {
      case 0 => Some(DisableStmtAll)
      case _ => Some(DisableStmtRules(words))
    }
  }
}
