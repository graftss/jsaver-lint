package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.js.ast.AST

trait LintComment

object LintComment {
  private val kinds = List(
    DisableNext,
    NamedDecl
  )

  def parse(commentBody: String, ast: Option[AST]): Option[LintComment] = {
    val words = commentBody.split("\\s+").filter(_.nonEmpty).toList
    println(s"words: ${words}")
    kinds.find(_.key == words.head).flatMap(_.parse(words.tail, ast))
  }
}

sealed trait LintCommentImpl {
  type Elem <: LintComment

  /** The string at the start of a comment body indicating this comment data.*/
  def key: String

  /**
   * Parse lint comment data from the body of a comment and the decorated AST node.
   *
   *  @param commentBody The whitespace-separated words in the comment body, not including the initial
   *                     key word indicating which comment subtype to parse.
   *  @param ast The AST node following the comment
   *  @return   The parsed lint comment data, if the parse was successful.
   */
  def parse(words: List[String], ast: Option[AST]): Option[Elem]
}

object DisableNext extends LintCommentImpl {
  val key: String = "lint-disable-next"

  trait DisableNextElem extends LintComment
  type Elem = DisableNextElem

  // Indicates that all rules should be disabled for the decorated AST node.
  case object DisableNextAll extends DisableNextElem

  // Indicates that only rules whose name is contained in `ruleNames`
  // should be disabled for the decorated AST node.
  case class DisableNextRules(ruleNames: List[String]) extends DisableNextElem

  override def parse(words: List[String], ast: Option[AST]): Option[DisableNextElem] = {
    words.length match {
      case 0 => Some(DisableNextAll)
      case _ => Some(DisableNextRules(words))
    }
  }
}

object NamedDecl extends LintCommentImpl {
  override def key: String = "lint-named-decl"

  trait NamedDeclElem extends LintComment
  type Elem = NamedDeclElem

  case class NamedClassDecl(name: String) extends NamedDeclElem
  case class NamedFuncDecl(name: String) extends NamedDeclElem

  override def parse(words: List[String], ast: Option[AST]): Option[NamedDeclElem] = {
    words.length match {
      case 0 => None
      case 1 if ast.exists(_.kind == "ClassDeclaration") => Some(NamedClassDecl(words.head))
      case 1 if ast.exists(_.kind == "FunctionDeclaration") => Some(NamedFuncDecl(words.head))
      case _ => None
    }
  }
}