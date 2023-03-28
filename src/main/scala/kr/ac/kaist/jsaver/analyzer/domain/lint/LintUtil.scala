package kr.ac.kaist.jsaver.analyzer.domain.lint

import kr.ac.kaist.jsaver.cfg.Call
import kr.ac.kaist.jsaver.ir.IApp

case class LintAlg(name: String) {

  // detects if a `Call` node is calling the algorithm referred to by `name`
  def hasAlgName(call: Call): Boolean = {
    call.inst match {
      case IApp(_, fexpr, _) => {
        fexpr.toString == name
      }
      case _ => false
    }
  }
}

object LintAlg {
  val PutValue = LintAlg("PutValue")
  val Call = LintAlg("Call")
  val Construct = LintAlg("Construct")
}

object LintUtil {
}
