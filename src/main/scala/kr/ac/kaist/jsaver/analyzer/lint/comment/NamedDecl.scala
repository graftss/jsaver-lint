package kr.ac.kaist.jsaver.analyzer.lint.comment

import kr.ac.kaist.jsaver.js.ast.AST

/** Tags a function or class declaration with a name that can be referenced by other lint rules. */
object NamedDecl extends LintCommentImpl {
  override def key: String = "lint-named-decl"

  trait NamedDeclElem extends LintComment
  type Elem = NamedDeclElem

  case class NamedClassDecl(name: String) extends NamedDeclElem
  case class NamedFuncDecl(name: String) extends NamedDeclElem

  override def parse(commentBody: String, words: List[String], ast: Option[AST]): Option[NamedDeclElem] = {
    words.length match {
      case 0 => None
      case 1 if ast.exists(_.kind == "ClassDeclaration") => Some(NamedClassDecl(words.head))
      case 1 if ast.exists(_.kind == "FunctionDeclaration") => Some(NamedFuncDecl(words.head))
      case _ => None
    }
  }
}
