package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.js.ast.AST

/** Disables one or more lint rules for the statement following the comment. */
object DisableNext extends LintCommentImpl {
  type Elem = DisableNextElem
  val key: String = "lint-disable-next"

  trait DisableNextElem extends LintComment {
    override def disablesRule(ruleName: String): Boolean = this match {
      case DisableNextAll => true
      case DisableNextRules(ruleNames) => ruleNames.contains(ruleName)
    }
  }

  // Indicates that all rules should be disabled for the decorated AST node.
  case object DisableNextAll extends DisableNextElem

  // Indicates that only rules whose name is contained in `ruleNames`
  // should be disabled for the decorated AST node.
  case class DisableNextRules(ruleNames: List[String]) extends DisableNextElem

  override def parse(commentBody: String, words: List[String], ast: Option[AST]): Option[DisableNextElem] = {
    words.length match {
      case 0 => Some(DisableNextAll)
      case _ => Some(DisableNextRules(words))
    }
  }
}
