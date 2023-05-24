package kr.ac.kaist.jsaver.analyzer.lint.comment
import kr.ac.kaist.jsaver.js.ast.AST

object DisableEval extends LintCommentImpl {
  trait DisableEvalElem extends LintComment {
    override def disablesRule(ruleName: String): Boolean = this match {
      case DisableEvalAll => true
      case DisableEvalRules(ruleNames) => ruleNames.contains(ruleName)
    }
  }
  type Elem = DisableEvalElem

  case object DisableEvalAll extends DisableEvalElem
  case class DisableEvalRules(ruleNames: List[String]) extends DisableEvalElem

  /** The string at the start of a comment body indicating the type of lint comment. */
  override def key: String = "lint-disable-eval"

  /**
   * Parse lint comment data from the body of a comment and the decorated AST node.
   *
   * @param commentBody The whitespace-separated words in the comment body, not including the initial
   *                    key word indicating which comment subtype to parse.
   * @param args: The whitespace-separated arguments to the comment (parsed from `commentBody`).
   * @param ast: The AST node following the comment
   * @return The parsed lint comment data, if the parse was successful.
   */
  override def parse(commentBody: String, args: List[String], ast: Option[AST]): Option[DisableEvalElem] = {
    args.length match {
      case 0 => Some(DisableEvalAll)
      case _ => Some(DisableEvalRules(args))
    }
  }
}
