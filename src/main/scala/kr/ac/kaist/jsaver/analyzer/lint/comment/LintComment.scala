package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.js.ast.AST

/** The underlying trait for all lint comment metadata. */
trait LintComment

object LintComment {
  private val kinds = List(
    DisableNext,
    NamedDecl
  )

  def parse(commentBody: String, ast: Option[AST]): Option[LintComment] = {
    val words = commentBody.split("\\s+").filter(_.nonEmpty).toList
    val result = kinds.find(_.key == words.head).flatMap(_.parse(commentBody, words.tail, ast))
    result
  }
}

trait LintCommentImpl {
  type Elem <: LintComment

  /** The string at the start of a comment body indicating the type of lint comment.*/
  def key: String

  /**
   * Parse lint comment data from the body of a comment and the decorated AST node.
   *
   *  @param commentBody The whitespace-separated words in the comment body, not including the initial
   *                     key word indicating which comment subtype to parse.
   *  @param ast The AST node following the comment
   *  @return   The parsed lint comment data, if the parse was successful.
   */
  def parse(commentBody: String, words: List[String], ast: Option[AST]): Option[Elem]
}