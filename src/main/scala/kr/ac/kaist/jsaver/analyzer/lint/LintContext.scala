package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.AbsSemantics

import scala.collection.mutable.ListBuffer

class LintContext(val sem: AbsSemantics) {
  var errors = 0
  var reports: ListBuffer[LintReport] = ListBuffer()

  // walk the script AST
  val walker = new LintWalker()
  walker.walk(sem.script)

  def report(report: LintReport): Unit = {
    errors += 1
    reports += report
  }

  def logReports(): Unit = {
    reports.zipWithIndex.foreach {
      case (report, idx) => {
        val disabled = if (report.disabled) "D" else ""
        println(s"[$idx$disabled] $report")
      }
    }
  }

  def logError(str: String): Unit = {
    println(s"Lint error: ${str}")
  }
}
