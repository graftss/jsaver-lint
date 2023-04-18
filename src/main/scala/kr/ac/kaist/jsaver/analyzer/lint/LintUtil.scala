package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.AbsSemantics
import kr.ac.kaist.jsaver.analyzer.domain.AbsLoc
import kr.ac.kaist.jsaver.cfg.CFG

import java.io._

object LintUtil {
  // Returns `true` if `callee` refers to a function object which may call a function object referred to a `caller`.
  def mayCall(sem: AbsSemantics, callee: AbsLoc, caller: AbsLoc): Boolean = {
    false
  }

  def generateAlgoUidFile(filename: String, cfg: CFG): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    cfg.funcs.foreach(func => {
      bw.write(s"Algo: ${func.name} [${func.uid}]\n")
      func.nodes.toList.sortBy(_.uid).foreach(node => {
        val step = node.getInst match {
          case Some(inst) if inst.line.isDefined => s"(${inst.line.get}) "
          case _ => ""
        }

        bw.write(s"  [${node.uid}] ${step}${node}\n")
      })
    })

    bw.flush()
  }
}
