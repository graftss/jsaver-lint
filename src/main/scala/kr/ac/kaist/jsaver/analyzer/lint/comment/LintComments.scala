package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableEval.{ DisableEvalAll, DisableEvalRules }
import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableStmt.{ DisableStmtAll, DisableStmtRules }
import kr.ac.kaist.jsaver.analyzer.lint.rule.LintRule

case class LintComments(comments: List[LintComment]) {
  /** Returns true if the lint rule `rule` is AST-disabled by these lint comments. */
  def isRuleAstDisabled(rule: LintRule): Boolean = {
    comments.exists {
      case DisableStmtAll => true
      case DisableStmtRules(rules) => rules.contains(rule.name)
      case _ => false
    }
  }

  /** Returns true if the lint rule `rule` is evak-disabled by these lint comments. */
  def isRuleEvalDisabled(rule: LintRule): Boolean = {
    comments.exists {
      case DisableEvalAll => true
      case DisableEvalRules(rules) => rules.contains(rule.name)
      case _ => false
    }
  }

  /** Print each lint comment on a separate line indented by `indent`. */
  def toString(indent: String = ""): String =
    s"\n$indent" + comments.map(_.toString).mkString(s"\n$indent")
}
