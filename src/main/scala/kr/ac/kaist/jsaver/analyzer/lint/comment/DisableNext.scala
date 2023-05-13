package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.js.ast.AST

object DisableNext extends LintCommentImpl {
  val key: String = "lint-disable-next"

  trait DisableNextElem extends LintComment
  type Elem = DisableNextElem

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
