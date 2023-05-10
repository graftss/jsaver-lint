package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.js.ast.AST

trait LintComment

object LintComment {
  private val kinds = List(
    DisableNext,
    NamedDecl
  )

  def parse(commentBody: String, ast: Option[AST]): Option[LintComment] = {
    for (commentKind <- kinds) {
      val result = commentKind.parse(commentBody, ast)
      if (result.isDefined) return result
    }

    None
  }
}

sealed trait LintCommentImpl {
  type Elem <: LintComment

  /**
   * Parse lint comment data from the body of a comment and the decorated AST node.
   *
   *  @param commentBody The body of the comment
   *  @param ast The AST node following the comment
   *  @return   The parsed lint comment data, if the parse was successful.
   */
  def parse(commentBody: String, ast: Option[AST]): Option[Elem]
}

object DisableNext extends LintCommentImpl {
  trait DisableNextElem extends LintComment
  type Elem = DisableNextElem

  // Indicates that all rules should be disabled for the decorated AST node.
  case object DisableNextAll extends DisableNextElem

  // Indicates that only rules whose name is contained in `ruleNames`
  // should be disabled for the decorated AST node.
  case class DisableNextRules(ruleNames: List[String]) extends DisableNextElem

  private val COMMENT_REGEX = "\\s*lint-disable-next(?:\\s+(\\w+))*".r

  override def parse(commentBody: String, ast: Option[AST]): Option[DisableNextElem] = {
    COMMENT_REGEX.findPrefixMatchOf(commentBody).map {
      case m if m.groupCount == 0 => DisableNextAll
      case m => DisableNextRules(m.subgroups)
    }
  }
}

object NamedDecl extends LintCommentImpl {
  trait NamedDeclElem extends LintComment
  type Elem = NamedDeclElem

  case class NamedClassDecl(name: String) extends NamedDeclElem
  case class NamedFuncDecl(name: String) extends NamedDeclElem

  private val COMMENT_REGEX = "\\s*lint-named-decl\\s+(\\w+)".r

  override def parse(commentBody: String, ast: Option[AST]): Option[NamedDeclElem] = {
    COMMENT_REGEX.findPrefixMatchOf(commentBody).flatMap {
      case m if ast.exists(_.kind == "ClassDeclaration") => Some(NamedClassDecl(m.group(1)))
      case m if ast.exists(_.kind == "FunctionDeclaration") => Some(NamedFuncDecl(m.group(1)))
      case _ => None
    }
  }
}