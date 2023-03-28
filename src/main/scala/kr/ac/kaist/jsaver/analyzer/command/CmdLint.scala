package kr.ac.kaist.jsaver.analyzer.command

import kr.ac.kaist.jsaver.analyzer._

// continue command
case object CmdLint extends Command(
  "lint", "Apply lint rules."
) {
  // options
  val options: List[String] = Nil

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String]
  ): Unit = {
    println("hello")
    cpOpt match {
      case Some(cp) => {
        println("cp = " + cp.toString)
        val lint = repl.sem.getState(cp).lint
        println("lint = " + lint.toString)
      }
      case None =>
    }
  }
}
