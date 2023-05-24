package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableStmt.{ DisableStmtAll, DisableStmtRules }
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule

case class LintComments(comments: List[LintComment]) {
  /** Returns true if the lint rule `rule` is disabled by these lint comments. */
  def isRuleDisabled(rule: LintRule): Boolean = {
    comments.exists {
      case DisableStmtAll => true
      case DisableStmtRules(rules) => rules.contains(rule.name)
      case _ => false
    }
  }

  /** Print each lint comment on a separate line indented by `indent`. */
  def toString(indent: String = ""): String =
    s"\n$indent" + comments.map(_.toString).mkString(s"\n$indent")
}
