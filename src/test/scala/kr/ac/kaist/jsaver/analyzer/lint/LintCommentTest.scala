package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.lint.comment.DisableNext.{DisableNextAll, DisableNextRules}
import kr.ac.kaist.jsaver.analyzer.lint.comment.LintComment
import kr.ac.kaist.jsaver.analyzer.lint.comment.NamedDecl.{NamedClassDecl, NamedFuncDecl}
import kr.ac.kaist.jsaver.js.Parser
import kr.ac.kaist.jsaver.js.ast.{AST, ClassDeclaration, FunctionDeclaration, FunctionDeclaration0}
import org.scalatest.FunSuite

class LintCommentTest extends FunSuite {
  def testComment(testDesc: String, commentBody: String, expected: LintComment, ast: Option[AST] = None): Unit = {
    test(testDesc) {
      LintComment.parse(commentBody, ast) match {
        case Some(observed) if observed == expected => ()
        case Some(lc) => fail(s"Parsed incorrect lint comment: ${lc}.")
        case _ => fail(s"Failed to parse lint comment: ${commentBody}")
      }
    }
  }

  val funcDecl: FunctionDeclaration = Parser.parse(Parser.FunctionDeclaration(List(false, false, false)), "function x() {}").get
  val classDecl: ClassDeclaration = Parser.parse(Parser.ClassDeclaration(List(false, false, false)), "class A {}").get

  testComment(
    "parse comment to disable all rules",
    "lint-disable-next   ",
    DisableNextAll
  )

  testComment(
    "parse comment to disable one rule",
    "lint-disable-next  no-dupe-keys  ",
    DisableNextRules(List("no-dupe-keys"))
  )

  testComment(
    "parse comment to disable multiple rules",
    "lint-disable-next  no-dupe-keys \n\n   array-callback-return ",
    DisableNextRules(List("no-dupe-keys", "array-callback-return"))
  )

  testComment(
    "parse comment to name a function declaration",
    "lint-named-decl react-component",
    NamedFuncDecl("react-component"),
    Some(funcDecl)
  )

  testComment(
    "parse comment to name a function declaration",
    "lint-named-decl react-component",
    NamedClassDecl("react-component"),
    Some(classDecl)
  )
}
