package kr.ac.kaist.jsaver.js.ast

import kr.ac.kaist.jsaver.js._
import kr.ac.kaist.jsaver.spec.algorithm._
import kr.ac.kaist.jsaver.ir._
import kr.ac.kaist.jsaver.error._
import kr.ac.kaist.jsaver.spec.grammar._
import kr.ac.kaist.jsaver.util.{ Pos, Span, WeakUId }
import kr.ac.kaist.jsaver.util.Useful.{ cached, error }
import io.circe._
import io.circe.syntax._

import scala.util.control.Breaks.{ break, breakable }

trait AST {
  var parent: Option[AST] = None
  def kind: String
  def idx: Int
  def k: Int
  def maxK: Int
  def span: Span
  def parserParams: List[Boolean]
  // child AST nodes
  def fullList: List[(String, PureValue)]

  def preComment: Option[String] = {
    if (span.preComment.isDefined) span.preComment
    else parent.flatMap(_.preComment)
  }

  // Compute the root node of the AST by recursively traveling upwards.
  def root: AST = {
    parent match {
      case None => this
      case Some(ast) => ast.root
    }
  }

  // The list of direct child AST nodes of this AST node
  def childAsts: List[AST] = fullList.flatMap {
    case (_, ASTVal(ast)) => Some(ast)
    case _ => None
  }

  def findKindAbove(kind: String): Option[AST] = {
    if (this.kind == kind) Some(this)
    else this.parent.flatMap(_.findKindAbove(kind))
  }

  def findKindBelow(kind: String): Option[AST] = {
    if (this.kind == kind) Some(this)
    else {
      childAsts.foreach(child => {
        child.findKindBelow(kind) match {
          case Some(ast) => return Some(ast)
          case _ => ()
        }
      })
      None
    }
  }

  // The list of identifier children of this AST
  def childIds: List[Identifier] =
    recChildIds.distinctBy(_.toString)

  private def recChildIds: List[Identifier] = {
    if (kind == "Identifier") List(this.asInstanceOf[Identifier])
    else if (fullList.isEmpty) List()
    else childAsts.flatMap(_.recChildIds)
  }

  def exprChild: Option[AST] = {
    if (kind.contains("Expression") || kind == "Initializer") {
      fullList.headOption match {
        case None => Some(this)
        case Some((_, ASTVal(child))) if child.kind.contains("Expression") => {
          child.exprChild
        }
        case Some((_, ASTVal(child))) => {
          Some(child)
        }
        case _ => Some(this)
      }
    } else {
      None
    }
  }

  // Compute the nearest ancestor that is a `FunctionBody` node.
  def nearestFnBody: AST = {
    parent match {
      case None => {
        //        println(s"??? null AST parent of [${this.kind}]: ${this}")
        this
      }
      case Some(parentAst) if parentAst.kind == "FunctionBody" => parentAst
      case Some(parentAst) if parentAst.kind == "ScriptBody" => parentAst
      case Some(parentAst) => parentAst.nearestFnBody
    }
  }

  def toLine(indent: Int = 0, newline: Boolean = true): String = {
    val code = this.toString
    val maxCodeLen = 75 - indent * 2
    val codeLine = if (code.length >= maxCodeLen) {
      code.substring(0, maxCodeLen) + " ..."
    } else {
      code
    }

    val commentStr = preComment.map(pc => s"[${pc}] ").getOrElse("")
    ("  " * indent) + s"$name ($hashCode): ${commentStr}$codeLine" + (if (newline) "\n" else "")
  }

  // print the AST node as a tree, with one node printed per line and depth
  // indicated by indentation
  def toTreeString(depth: Int = 0, collapseExpr: Boolean = true): String = {
    val spaces = "  " * depth

    if (kind == "StatementList" && idx == 1) {
      // special case to display StatementList1 as a flat list
      var node = this
      var result = List[String]()

      breakable {
        while (node.kind == "StatementList" && idx == 1) {
          node.fullList match {
            case (_, ASTVal(nextNode)) :: (_, ASTVal(stmt)) :: _ => {
              // two children means there's another statement before `stmt`
              result ::= stmt.toTreeString(depth + 1)
              node = nextNode
            }
            case (_, ASTVal(stmt)) :: _ => {
              // one child means that `stmt` is the first statement in the list,
              // so iteration can stop
              result ::= stmt.toTreeString(depth + 1)
              break
            }
            case _ => break
          }
        }
      }

      toLine(depth) + result.mkString("\n")
    } else if (collapseExpr && fullList.length == 1 && kind.contains("Expression")) {
      // collapse expression chains
      fullList.head match {
        case (_, ASTVal(ast)) => ast.toTreeString(depth)
        case _ => ""
      }
    } else if (fullList.nonEmpty) {
      // normal AST node with children
      toLine(depth) + fullList.flatMap({
        case (_, ASTVal(ast)) => List(ast.toTreeString(depth + 1))
        case _ => List()
      }).mkString("\n")
    } else {
      // normal AST node without children
      toLine(depth, false)
    }
  }

  // not use helpers of case calsses
  override def hashCode: Int = super.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: AST => this eq that
    case _ => false
  }

  // name
  def name: String = kind + idx

  // to JSON format
  def toJson: Json = Json.arr(
    Json.fromInt(idx),
    Json.arr(fullList.map {
      case (_, ASTVal(ast)) => ast.toJson
      case _ => Json.Null
    }: _*),
    Json.arr(parserParams.map(p => Json.fromInt(if (p) 1 else 0)): _*),
    Json.arr(
      Json.fromInt(-1), Json.fromInt(-1), Json.fromInt(-1),
      Json.fromInt(-1), Json.fromInt(-1), Json.fromInt(-1),
    )
  )

  // pretty printer
  def prettify: Json = Json.arr(
    Json.fromString(s"$kind[$idx,$k]"),
    Json.arr(fullList.map {
      case (_, ASTVal(ast)) => ast.prettify
      case _ => Json.Null
    }: _*),
    Json.arr(parserParams.map(Json.fromBoolean): _*),
  )

  // get possible kinds
  def getKinds: Set[String] = (list match {
    case List((_, ASTVal(ast))) => ast.getKinds
    case _ => Set()
  }) ++ Set(kind)

  // get element list for the given kind
  def getElems(given: String): List[AST] = {
    if (given == kind) List(this)
    else list.foldLeft(List[AST]()) {
      case (l, (_, ASTVal(ast))) => l ++ ast.getElems(given)
      case (l, _) => l
    }
  }

  // list of actual values
  lazy val list: List[(String, PureValue)] = fullList.filter {
    case (_, Absent) => false
    case _ => true
  }

  // children
  def children: List[PureValue] = list.map(_._2)

  // semantic map
  def semMap: Map[String, Algo] = AST.getSemMap((kind, idx))

  // get semantics
  def semantics(fname: String): Option[(Algo, List[PureValue])] = {
    semMap.get(fname + k.toString) match {
      case Some(f) => Some((f, ASTVal(this) :: fullList.map(_._2)))
      case None if fname == "Contains" => Some((defaultContains, List(ASTVal(this))))
      case None => list match {
        case List((_, ASTVal(x))) => x.semantics(fname)
        case _ => None
      }
    }
  }

  // existence check
  def exists(kindFilter: String => Boolean): Boolean = kindFilter(kind) || list.exists {
    case (_, ASTVal(ast)) => ast.exists(kindFilter)
    case _ => false
  }

  // get sub-AST
  def subs(name: String): Option[PureValue] = list.toMap.get(name)

  // Helpers
  protected def d(x: Any, n: Int): Int = x match {
    case Some(_) => 2 * n + 1
    case None => 2 * n
    case _ => n
  }
  protected def l(name: String, x: Any, list: List[(String, PureValue)]): List[(String, PureValue)] = x match {
    case Some(a: AST) => (name.substring(7, name.length - 1), ASTVal(a)) :: list
    case None => (name.substring(7, name.length - 1), Absent) :: list
    case a: AST => (name, ASTVal(a)) :: list
    case _ => list
  }

  // check supported syntax
  def checkSupported: AST = AST.checkSupported(this)
}
object AST {
  // check supported syntax
  private val notSupportedSyntaxPrefixList = List("RegularExpression")
  def checkSupported(ast: AST): AST = {
    ast.exists(name => notSupportedSyntaxPrefixList.exists(pre => {
      if (name.startsWith(pre)) throw NotSupported(pre)
      false
    }))
    ast
  }

  private val getSemMap = cached[(String, Int), Map[String, Algo]] {
    case (kind, idx) => {
      val pattern = s"${kind}\\[${idx},(\\d+)\\]\\.(.*)".r
      (for {
        (name, algo) <- algoMap
        methodName <- name match {
          case pattern(j, methodName) => Some(methodName + j)
          case _ => None
        }
      } yield methodName -> algo).toMap
    }
  }

  // compressed AST data
  trait Compressed {
    // equality of two compressed objects
    def equals(that: Compressed): Boolean = (this, that) match {
      case (c0: NormalCompressed, c1: NormalCompressed) =>
        // ignore span info
        val b0 = c0.idx == c1.idx
        val b1 = c0.subs.size == c1.subs.size &&
          c0.subs.zip(c1.subs).map {
            case (None, None) => true
            case (Some(s0), Some(s1)) => s0.equals(s1)
            case _ => false
          }.forall(_ == true)
        val b2 = c0.params.size == c1.params.size &&
          c0.params.zip(c1.params).map { case (p0, p1) => p0 == p1 }.forall(_ == true)
        b0 && b2
      case (LexicalCompressed(k0, s0), LexicalCompressed(k1, s1)) =>
        s0 == s1 && k0 == k1
      case _ => false
    }
  }
  case class NormalCompressed(
    idx: Int,
    subs: Array[Option[Compressed]],
    params: List[Boolean],
    span: Span
  ) extends Compressed
  case class LexicalCompressed(
    kind: String,
    str: String
  ) extends Compressed

  // convert json to compressed form
  def apply(data: Json): Option[Compressed] = data match {
    case arr if data.isArray => arr.asArray.get.toList match {
      // non-lexical
      case List(jIdx, jSubs, jParams, jSpan) =>
        val idx = jIdx.asNumber.get.toInt.get
        val subs = jSubs.asArray.get.toArray.map(AST(_))
        val params = jParams.asArray.get.toList.map(_.asNumber.get.toInt.get == 1)
        val List(sl, sc, si, el, ec, ei) =
          jSpan.asArray.get.toList.map(_.asNumber.get.toInt.get)
        Some(NormalCompressed(idx, subs, params, Span(Pos(sl, sc, si), Pos(el, ec, ei))))
      // lexical
      case List(jKind, jStr) =>
        val kind = jKind.asString.get
        val str = jStr.asString.get
        Some(LexicalCompressed(kind, str))
    }
    case none if data.isNull => None
    case _ => error("invalid AST compressed form")
  }
}
