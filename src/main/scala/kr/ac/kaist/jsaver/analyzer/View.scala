package kr.ac.kaist.jsaver.analyzer

import kr.ac.kaist.jsaver.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.jsaver.cfg._
import kr.ac.kaist.jsaver.ir.ASTVal
import kr.ac.kaist.jsaver.js.ast._

case class CallView(call: Call, astOpt: Option[AST]) {
  //    override def equals(o: Any): Boolean = o match {
  //      case o: CallView => o.call.equals(call)
  //      case _ => false
  //    }
}

case class JSCallToken(ast: AST, value: AbsValue) {
  override def equals(o: Any): Boolean = o match {
    case o: JSCallToken => o.ast.equals(ast)
    case _ => false
  }

  override def toString: String = ast.toString
}

// view abstraction for analysis sensitivities
// stringifier: `ViewApp` in `analyzer/Stringifier.scala`
case class View(
  jsViewOpt: Option[JSView] = None,
  calls: List[CallView] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0
) extends AnalyzerElem {
  // get JavaScript contexts
  def jsCalls: List[JSCallToken] = jsViewOpt.fold(List[JSCallToken]())(_.calls)
  def jsLoops: List[LoopCtxt] = jsViewOpt.fold(List[LoopCtxt]())(_.loops)

  // get ir ijk
  def getIrIJK: (Int, Int, Int) = (
    if (loops.isEmpty) 0 else loops.map(_.depth).max,
    loops.length,
    calls.length
  )
  // get js ijk
  def getJsIJK: (Int, Int, Int) = jsViewOpt.fold((-1, -1, -1))(js => (
    if (js.loops.isEmpty) 0 else js.loops.map(_.depth).max,
    js.loops.length,
    js.calls.length
  ))

  // Map a `View` to its JS function call string
  def jsCallString(): Option[String] = {
    jsViewOpt.map(jsView => {
      jsView.calls.flatMap {
        case JSCallToken(ast, _) if ast.kind == "Initializer" => ast.children.head match {
          case ASTVal(ast) => Some(ast)
          case _ => None
        }
        case ast => Some(ast)
      }.reverse.mkString(" -> ")
    })
  }

  def toVerboseString: String = {
    val sep = "\n  "
    val sep2 = "\n    "
    val irStr = s"View: $sep" +
      s"calls:$sep2${calls.map(_.call).mkString(sep2)}$sep" +
      s"loops:$sep2${loops.mkString(sep2)}"

    val jsStr = jsViewOpt match {
      case Some(jsView) =>
        sep +
          s"js calls:$sep2${jsView.calls.mkString(sep2)}$sep" +
          s"js loops:$sep2${jsView.loops.mkString(sep2)}"
      case None => ""
    }

    irStr + jsStr
  }
}

// contexts
case class LoopCtxt(loop: Loop, depth: Int)

// views for JavaScript
case class JSView(
  ast: AST,
  calls: List[JSCallToken],
  loops: List[LoopCtxt]
)
