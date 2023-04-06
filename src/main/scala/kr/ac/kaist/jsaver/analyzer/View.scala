package kr.ac.kaist.jsaver.analyzer

import kr.ac.kaist.jsaver.cfg._
import kr.ac.kaist.jsaver.js.ast._

// view abstraction for analysis sensitivities
// stringifier: `ViewApp` in `analyzer/Stringifier.scala`
case class View(
  jsViewOpt: Option[JSView] = None,
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0
) extends AnalyzerElem {
  // get JavaScript contexts
  def jsCalls: List[AST] = jsViewOpt.fold(List[AST]())(_.calls)
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

  def toCallStackString: String = jsViewOpt match {
    case Some(JSView(ast, calls, loops)) => {
      (ast :: calls).reverse.mkString(" -> ")
    }
    case None => ""
  }
}

// contexts
case class LoopCtxt(loop: Loop, depth: Int)

// views for JavaScript
case class JSView(
  ast: AST,
  calls: List[AST],
  loops: List[LoopCtxt]
)
