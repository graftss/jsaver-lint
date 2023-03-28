package kr.ac.kaist.jsaver.analyzer.domain

import kr.ac.kaist.jsaver.analyzer.View
import kr.ac.kaist.jsaver.util.Appender.App
import kr.ac.kaist.jsaver.util.Useful.stringify

case class MayCallees(callees: List[Int]) {
  def apply(callees: List[Int]): MayCallees = MayCallees(callees)

  def addCallee(hash: Int): MayCallees =
    MayCallees(callees :+ hash)
}

object LintDomain extends Domain {
  // (view at call entry, hash of callee function AST)
  case class CallCtx(view: View, hash: Int)
  type MayCallMap = Map[CallCtx, MayCallees]

  lazy val Bot = Elem(Map())
  lazy val Top = Elem(Map())

  implicit val app: App[Elem] = (app, elem) => app >> (elem match {
    case Bot => "⊥"
    case Top => "T"
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
    def ⊔(that: Elem): Elem = that

    // conversion to string
    override def toString: String = stringify(this)

    def recordCall(ctxView: View, ctxHash: Int, calleeHash: Int) = {
      val ctx = CallCtx(ctxView, ctxHash)
      val nextCallees = mayCall.get(ctx) match {
        case Some(callees) => callees.addCallee(calleeHash)
        case None => MayCallees(List(calleeHash))
      }

      mayCall.updated(CallCtx(ctxView, ctxHash), nextCallees)
    }

    def getCalls(hash: Int): String = {
      mayCall.filter(k => k._1.hash == hash)
        .mkString(s"Calls for hash $hash:\n", ", ", "\n")
    }
  }
}
