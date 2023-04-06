package kr.ac.kaist.jsaver.lint

import kr.ac.kaist.jsaver.js.ASTWalker
import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.util.Span

case class FuncDefInfo(bodyHash: Int, name: Option[String], span: Span)

class LintWalker extends ASTWalker {
  // maps hashes of function definition bodies to relevant data about each definition
  var funcDefs: Map[Int, FuncDefInfo] = Map()

  // attempt to parse a `FuncDefInfo` object from an AST node
  def parseFuncDefInfo(ast: AST): Option[FuncDefInfo] = ast match {
    case Script0(Some(x0), _, span) =>
      Some(FuncDefInfo(x0.hashCode, Some("File"), span))
    case FunctionDeclaration0(x1, _, x6, _, span) =>
      Some(FuncDefInfo(x6.hashCode, Some(x1.toString), span))
    case FunctionDeclaration1(_, x5, _, span) =>
      Some(FuncDefInfo(x5.hashCode, None, span))
    case FunctionExpression0(x1, _, x6, _, span) =>
      Some(FuncDefInfo(x6.hashCode, x1.map(_.toString), span))
    case _ => None
  }

  // record a `FuncDefInto` object from a `Script` node
  override def walk(ast: Script): Unit = {
    parseFuncDefInfo(ast).foreach(info => funcDefs += info.bodyHash -> info)
    super.walk(ast)
  }

  // record a `FuncDefInfo` object from a `FunctionDeclaration` node
  override def walk(ast: FunctionDeclaration): Unit = {
    parseFuncDefInfo(ast).foreach(info => funcDefs += info.bodyHash -> info)
    super.walk(ast)
  }

  // record a `FuncDefInfo` object from a `FunctionExpression` node
  override def walk(ast: FunctionExpression): Unit = {
    parseFuncDefInfo(ast).foreach(info => funcDefs += info.bodyHash -> info)
    super.walk(ast)
  }
}
