package kr.ac.kaist.jsaver.analyzer.domain

import kr.ac.kaist.jsaver.analyzer.View
import kr.ac.kaist.jsaver.util.Appender.App
import kr.ac.kaist.jsaver.util.Useful.stringify

case class MayCallees(callees: List[Int]) {
  def apply(callees: List[Int]): MayCallees = MayCallees(callees)

  def addCallee(hash: Int): MayCallees =
    MayCallees(callees :+ hash)
}

case class CallCtx(view: View, hash: Int) {
  override def toString: String = s"CallCtx(view=${view.toString(false)}, hash=$hash)"
}

object LintDomain extends Domain {
  // (view at call entry, hash of caller function body AST)
  type MayCallMap = Map[CallCtx, MayCallees]

  lazy val Bot = Elem(Map())
  lazy val Top = Elem(Map())

  implicit val app: App[Elem] = (app, elem) => app >> (elem match {
    case Bot => "⊥"
    case Top => "T"
    case Elem(mayCall) => s"MayCall($mayCall)"
  })

  // constructors
  def apply(
    mayCall: MayCallMap = Map()
  ): Elem = Elem(mayCall)

  // extractors
  def unapply(elem: Elem) = Some((
    elem.mayCall,
  ))

  case class Elem(
    mayCall: MayCallMap
  ) extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match {
      case (Bot, _) | (_, Top) => true
      case (_, Bot) | (Top, _) => false
      case _ => true // TODO
    }

    // TODO: join operator
    def ⊔(that: Elem): Elem = {
      println(s"joining lint states: ${this} || ${that}")
      that
    }

    // conversion to string
    override def toString: String = {
      stringify(this)
    }

    def recordCall(ctxView: View, callerHash: Int, calleeHash: Int) = {
      val ctx = CallCtx(ctxView, callerHash)
      val nextCallees = mayCall.get(ctx) match {
        case Some(callees) => callees.addCallee(calleeHash)
        case None => MayCallees(List(calleeHash))
      }

      this.copy(mayCall = mayCall.updated(CallCtx(ctxView, callerHash), nextCallees))
    }

    def getCalls(hash: Int): String = {
      mayCall.filter(k => k._1.hash == hash)
        .mkString(s"Calls for hash $hash:\n", ", ", "\n")
    }
  }
}
