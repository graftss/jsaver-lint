package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.lint.DisableNext.DisableNextAll
import org.scalatest.FunSuite

class LintCommentTest extends FunSuite {
  test("parse comment to disable all rules") {
    val commentBody = "lint-disable-next   "
    LintComment.parse(commentBody, None) match {
      case Some(DisableNextAll) => ()
      case _ => fail()
    }
  }
}
