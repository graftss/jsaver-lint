package kr.ac.kaist.jsaver.analyzer

import kr.ac.kaist.jsaver.analyzer.domain._
import kr.ac.kaist.jsaver.LINE_SEP
import kr.ac.kaist.jsaver.cfg
import kr.ac.kaist.jsaver.cfg.{ Stringifier => _, _ }
import kr.ac.kaist.jsaver.js.ast.AST
import kr.ac.kaist.jsaver.util.Appender
import kr.ac.kaist.jsaver.util.Appender._
import kr.ac.kaist.jsaver.util.Useful._

// Analyzer Stringifier
class Stringifier(
  detail: Boolean = true,
  line: Boolean = false,
  asite: Boolean = false
) {
  // load other stringifiers
  val cfgStringifier = new cfg.Stringifier(detail, line, asite)
  import cfgStringifier._, irStringifier._

  // analyzer components
  implicit lazy val AnalyzerElemApp: App[AnalyzerElem] = (app, comp) => comp match {
    case comp: ControlPoint => ControlPointApp(app, comp)
    case comp: View => ViewApp(app, comp)
    case comp: AbsRefValue => AbsRefValueApp(app, comp)
  }

  // control points
  implicit lazy val ControlPointApp: App[ControlPoint] = (app, cp) => cp match {
    case NodePoint(node, view) => app >> view >> ":" >> node
    case ReturnPoint(func, view) => app >> view >> ":RET:" >> func.uidString
  }

  // views
  implicit lazy val ViewApp: App[View] = (app, view) => {
    def ctxtStr(
      calls: List[String],
      loops: List[LoopCtxt]
    ): Appender = if (detail) {
      app >> calls.mkString(s"[call(${calls.length}): ", ", ", "]")
      app >> loops.map(_ match {
        case LoopCtxt(loop, depth) => s"${loop.uidString}($depth)"
      }).mkString("[loop: ", ", ", "]")
    } else {
      app >> "[call: " >> calls.length >> "]"
      app >> "[loop: " >> loops.length >> "]"
    }

    // js views
    view.jsViewOpt.map {
      case JSView(ast, calls, loops) =>
        app >> s"☊[${ast.kind}/${ast.hashCode}]"
        ctxtStr(calls.map(call => s"☊[${call.kind}/${call.hashCode}]"), loops)
    }

    // ir contexts

    // original code
    // ctxtStr(view.calls.map(call => s"${call.uidString}/${call.inst}"), view.loops)
    ctxtStr(view.calls.map(cv => s"${cv.call.inst.toMyString}"), view.loops)
  }

  // abstract reference values
  implicit lazy val AbsRefValueApp: App[AbsRefValue] = (app, ref) => ref match {
    case AbsRefId(id) => app >> id
    case AbsRefProp(base, prop) => app >> base >> "[" >> prop >> "]"
  }
}
