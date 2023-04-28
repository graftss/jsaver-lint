package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.analyzer.{ AbsSemantics, JSCallToken }
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsLoc, AbsObj, AbsState, AbsValue }
import kr.ac.kaist.jsaver.cfg.CFG

import java.io._

object LintUtil {
  def locTypeName(st: AbsState, value: AbsValue): Option[String] =
    st(value.loc).map(_.getTy.name)

  // Returns `true` if `callee` refers to a function object which may call a function object referred to a `caller`.
  def mayCall(sem: AbsSemantics, callee: AbsValue, caller: AbsValue): Boolean = {
    val node = sem.cfg.funcMap("Call").nodes.find(_.uid == 4068).get
    sem.npMap.filter(pair => pair._1.node.uid == node.uid).foreach {
      case (np, st) => {
        val view = np.view
        np.view.jsViewOpt match {
          case Some(jsView) => {
            // the callee is the head of the js view's `calls` list
            jsView.calls.headOption.map {
              case JSCallToken(_, value) if !locTypeName(st, value).contains("BuiltinFunctionObject") => {
                // if the callee isn't a builtin function (i.e. it's a user-defined function)
                //                st(value.loc).map(absObj => )
                None
              }
              case _ => None
            }
            //            println(s"callee loc: ${callee.value.loc}")
            //            println(s"callee obj: ${(st(callee.value.loc).get)(AbsValue("SourceText"))}")
            jsView.calls.foreach {
              case JSCallToken(ast, calleeValue) => {
                val funcObj = st(calleeValue.loc).get
                val sourceText = funcObj(AbsValue("SourceText"))
                println(s"  ast:${ast} --> value: ${sourceText}")
              }
            }
          }
          case None => ()
        }
      }
    }
    false
  }

  def generateAlgoUidFile(filename: String, cfg: CFG): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    cfg.funcs.foreach(func => {
      bw.write(s"Algo: ${func.name} [${func.uid}]\n")
      func.nodes.toList.sortBy(_.uid).foreach(node => {
        val step = node.getInst match {
          case Some(inst) if inst.line.isDefined => s"(${inst.uid}/${inst.line.get}) "
          case Some(inst) => s"(${inst.uid})"
          case _ => ""
        }

        bw.write(s"  [${node.uid}] ${step}${node}\n")
      })
    })

    bw.flush()
  }

  def writeToFile(filename: String, str: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(str)
    bw.flush()
  }
}
