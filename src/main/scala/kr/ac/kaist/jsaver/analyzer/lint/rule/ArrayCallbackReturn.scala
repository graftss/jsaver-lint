package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsValue, SimpleDomain }
import kr.ac.kaist.jsaver.analyzer.{ AbsSemantics, NodePoint }
import kr.ac.kaist.jsaver.analyzer.lint.{ FuncDefInfo, LintContext, LintReport }
import kr.ac.kaist.jsaver.cfg.{ CFG, Call, InstNode, Linear, Node }
import kr.ac.kaist.jsaver.ir.{ ILet, Id }
import kr.ac.kaist.jsaver.js.ast.AST

case class AcrInst(name: String, node: Node, id: Id)

case class AcrReport(override val message: String) extends LintReport {
  override val rule: LintRule = ArrayCallbackReturn
}

object ArrayCallbackReturn extends LintRule {
  val name = "array-callback-return"

  def findInstrumentedInsts(cfg: CFG): List[AcrInst] = {
    var result: List[AcrInst] = List()

    // Array.prototype.map instrumented instruction:
    // 11:app __x8__ = (CreateDataPropertyOrThrow A Pk mappedValue)
    val mapNode = cfg.funcMap("GLOBAL.Array.prototype.map").nodes.find {
      case node: Call if node.inst.line.contains(11) => true
      case _ => false
    }.get
    result ::= AcrInst("map", mapNode, Id("mappedValue"))

    result
  }

  def validateAcrInst(ctx: LintContext, acrInst: AcrInst): Unit = {
    val states = ctx.sem.npMap.filter(_._1.node == acrInst.node)
    states.keySet.foreach(np => {
      // read the value returned by the array callback from the program state at the given acr instruction
      val st = ctx.sem.getState(np)
      val value = st(acrInst.id, np)

      // if the value may be undefined, report a lint error
      if (!value.undef.isBottom) {
        // read the callback function (which is an argument of the array method) from the program state
        val callbackDef = st(st(Id("callbackfn"), np).loc) match {
          case Some(obj) => {
            // read the callback function objects's `ECMAScriptCode` field, which yields a
            // reference to the AST of its `FunctionBody` node
            obj(AbsValue("ECMAScriptCode")).getSingleAst match {
              case Some(ast) => {
                Some(ctx.walker.funcDefs(ast.hashCode))
              }
              case None => {
                ctx.logError("ArrayCallbackReturn error: cannot read AST of callback function")
                None
              }
            }
          }
          case None => {
            ctx.logError("ArrayCallbackReturn error: cannot read `callbackAddr`")
            None
          }
        }

        // read the array method callite from the control point's view.
        val callsite = np.view.jsViewOpt.map(_.ast)

        ctx.report(AcrReport(reportMessage(acrInst, callbackDef, callsite)))
      }
    })
  }

  def reportMessage(acrInst: AcrInst, callbackDef: Option[FuncDefInfo], callsite: Option[AST]): String = {
    val name = callbackDef.map(_.getName).getOrElse("[callback name]")
    val callsiteStr = callsite.map(_.toString).getOrElse("[callsite AST]")

    s"Returned undefined from callback ${name} to array method `${acrInst.name}`.\n" +
      s"\t--> ${callsiteStr}"
  }

  override def validate(ctx: LintContext): Unit = {
    findInstrumentedInsts(ctx.sem.cfg).foreach(validateAcrInst(ctx, _))
  }
}
