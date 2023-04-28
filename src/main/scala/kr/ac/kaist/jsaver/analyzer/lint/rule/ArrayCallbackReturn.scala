package kr.ac.kaist.jsaver.analyzer.lint.rule
import kr.ac.kaist.jsaver.analyzer.domain.{ AbsObj, AbsState, AbsValue, BasicObj, SimpleDomain }
import kr.ac.kaist.jsaver.analyzer.{ AbsSemantics, NodePoint }
import kr.ac.kaist.jsaver.analyzer.lint.{ FuncDefInfo, LintContext, LintError, LintReport, LintSeverity, LintUtil }
import kr.ac.kaist.jsaver.cfg.{ CFG, Call, InstNode, Linear, Node }
import kr.ac.kaist.jsaver.ir.{ IApp, ILet, Id }
import kr.ac.kaist.jsaver.js.ast.AST

// Data characterizing an algorithm step that receives the return value of an array callback.
case class CallbackArg(algoIdName: String, name: String)

// Data characterizing an instruction in an array method's algorithm where the return value of that
// array method's callback argument can be inspected.
case class AcrInstId(algoName: String, step: Int, valueIdName: String, args: List[CallbackArg])

// Data characterizing a CFG node that receives the return value of an array callback.
// Each `AcrInstId` value is mapped to an `AcrInst` using a spec CFG.
case class AcrInst(id: AcrInstId, methodName: String, node: Node, valueId: Id) {
  // read the argument values passed to the callback from the state
  def argValues(np: NodePoint[Node], st: AbsState): List[AbsValue] =
    id.args.map(arg => st(Id(arg.algoIdName), np))

  def argsStr(np: NodePoint[Node], st: AbsState, indent: String = "  "): String =
    id.args.zip(argValues(np, st)).map {
      case (CallbackArg(_, name), value) => s"$indent$name argument: $value"
    }.mkString("\n")
}

case class AcrReport(np: NodePoint[Node], st: AbsState, acrInst: AcrInst, callbackDef: Option[FuncDefInfo]) extends LintReport {
  override val rule: LintRule = ArrayCallbackReturn
  override val severity: LintSeverity = LintError

  private def callsiteEnv(np: NodePoint[Node], st: AbsState): Option[AbsObj] = {
    val stackRef = st(Id("EXECUTION_STACK"), np)

    st(stackRef.loc).flatMap {
      case elem: BasicObj.KeyWiseList if elem.values.length >= 2 => {
        val ctxRef = elem.values(elem.values.length - 2)
        val ctx = st(ctxRef.loc).get

        st(ctx("LexicalEnvironment").comp.normal.value.loc)
      }
      case _ => None
    }
  }

  override def message: String = {
    val name = "callback" + callbackDef.map(cb => " " + cb.getName).getOrElse("")

    val xxx = callsiteEnv(np, st).map(readEnvValue(st, _, "xxx"))
    println(s"xxx: ${xxx}")
    val yyy = callsiteEnv(np, st).map(readEnvValue(st, _, "yyy"))
    println(s"yyy: $yyy")

    List(
      s"Returned `undefined` from ${name} to array method `${acrInst.methodName}`:",
      callStringStr(np),
      viewAstStr(np, "callsite"),
      acrInst.argsStr(np, st),
    ).mkString("\n")
  }
}

object ArrayCallbackReturn extends LintRule {
  val name = "array-callback-return"

  val defaultCbArgs = List(
    CallbackArg("kValue", "item"),
    CallbackArg("k", "index")
  )

  val reduceCbArgs = List(
    CallbackArg("accumulator", "accumulator"),
    CallbackArg("kValue", "item"),
    CallbackArg("k", "index")
  )

  val acrInstIds = List(
    // Array.prototype.map instrumented instruction:
    // 10:let mappedValue = [? __x7__]
    AcrInstId("GLOBAL.Array.prototype.map", 10, "__x7__", defaultCbArgs),

    // Array.prototype.reduce instrumented instruction:
    // 22:accumulator = [? __x9__]
    AcrInstId("GLOBAL.Array.prototype.reduce", 22, "__x9__", reduceCbArgs),

    // Array.prototype.filter instrumented instruction:
    // 11:let selected = [! __x8__]
    AcrInstId("GLOBAL.Array.prototype.filter", 11, "__x7__", defaultCbArgs)
  )

  // Maps the list of `AcrInstId` values to a list of `AcrInst` values using a spec CFG.
  def findInstrumentedInsts(cfg: CFG): List[AcrInst] = {
    // We can extract the array method name from its algorithm's name by removing the prefix below
    val methodNameStrIdx = "GLOBAL.Array.prototype.".length

    // For each `AcrInstId` value:
    acrInstIds.flatMap {
      case id @ AcrInstId(algoName, instLine, valueIdName, cbArgNames) => {
        // Find the corresponding node in the spec CFG
        val nodeOpt = cfg.funcMap(algoName).nodes.find {
          case node: InstNode if node.inst.line.contains(instLine) => {
            node.inst match {
              case IApp(_, _, _) => false
              case _ => true
            }
          }
          case _ => false
        }

        // If the node exists, use it to create an `AcrInst` value.
        nodeOpt match {
          case Some(node) => List(
            AcrInst(id, algoName.substring(methodNameStrIdx), node, Id(valueIdName))
          )
          case None => List()
        }
      }
    }
  }

  def simplifyIdValue(value: AbsValue): AbsValue = {
    if (!value.comp.isBottom) {
      value.comp.normal.value
    } else {
      value
    }
  }

  def validateAcrInst(ctx: LintContext, acrInst: AcrInst): Unit = {
    val states = ctx.sem.npMap.filter(_._1.node == acrInst.node)
    states.keySet.foreach(np => {
      // read the value returned by the array callback from the program state at the given acr instruction
      val st = ctx.sem.getState(np)
      // since calling the callback returns a completion, extract the normal value from the completion
      val value = simplifyIdValue(st(acrInst.valueId, np))

      // if the value may be undefined, report a lint error
      if (!value.undef.isBottom) {
        // read the callback function (which is an argument of the array method) from the program state
        val callbackDef = st(st(Id("callbackfn"), np).loc) match {
          case Some(obj) => {
            // read the callback function object's `ECMAScriptCode` field, which yields a
            // reference to the AST of its `FunctionBody` node
            obj(AbsValue("ECMAScriptCode")).getSingleAst match {
              case Some(ast) => {
                ctx.walker.funcDefs.get(ast.hashCode)
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

        ctx.report(AcrReport(np, st, acrInst, callbackDef))
        LintUtil.writeToFile("acr-state.txt", st.toString)
        println("wrote state to acr-state.txt")
      }
    })
  }

  override def validate(ctx: LintContext): Unit = {
    findInstrumentedInsts(ctx.sem.cfg).foreach(validateAcrInst(ctx, _))
  }
}
