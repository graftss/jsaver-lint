package kr.ac.kaist.jsaver.lint

import kr.ac.kaist.jsaver.js.ASTWalker
import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.util.Span

// data recorded for each function definition in a program
case class FuncDefInfo(bodyHash: Int, name: Option[String], span: Span)

// data recorded for each mutation expression in a program
case class MutationInfo(exprHash: Int, span: Span)

class LintWalker extends ASTWalker {
  // maps hashes of function definition bodies to relevant data about each definition
  var funcDefs: Map[Int, FuncDefInfo] = Map()
  var mutations: Map[Int, MutationInfo] = Map()

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

  def recordFuncDefInfo(ast: AST): Unit = {
    parseFuncDefInfo(ast).foreach(info => funcDefs += info.bodyHash -> info)
  }

  // record a `FuncDefInto` object from a `Script` node
  override def walk(ast: Script): Unit = {
    recordFuncDefInfo(ast)
    super.walk(ast)
  }

  // record a `FuncDefInfo` object from a `FunctionDeclaration` node
  override def walk(ast: FunctionDeclaration): Unit = {
    recordFuncDefInfo(ast)
    super.walk(ast)
  }

  // record a `FuncDefInfo` object from a `FunctionExpression` node
  override def walk(ast: FunctionExpression): Unit = {
    recordFuncDefInfo(ast)
    super.walk(ast)
  }

  def parseMutationInfo(ast: AST): Option[MutationInfo] = {
    ast match {
      // LeftHandSideExpression "=" AssignmentExpression
      case AssignmentExpression4(x0, x2, parserParams, span) =>
        Some(MutationInfo(ast.hashCode, span))

      // LeftHandSideExpression AssignmentOperator AssignmentExpression
      case AssignmentExpression5(x0, x1, x2, parserParams, span) =>
        Some(MutationInfo(ast.hashCode, span))

      // LeftHandSideExpression "&&=" AssignmentExpression
      case AssignmentExpression6(x0, x1, parserParams, span) =>
        Some(MutationInfo(ast.hashCode, span))

      // LeftHandSideExpression "||=" AssignmentExpression
      case AssignmentExpression7(x0, x1, parserParams, span) =>
        Some(MutationInfo(ast.hashCode, span))

      // LeftHandSideExpression "??=" AssignmentExpression
      case AssignmentExpression8(x0, x1, parserParams, span) =>
        Some(MutationInfo(ast.hashCode, span))

      case _ => None
    }
  }

  def recordMutationInfo(ast: AST): Unit = {
    parseMutationInfo(ast).foreach(info => mutations += info.exprHash -> info)
  }

  override def walk(ast: AssignmentExpression): Unit = {
    recordMutationInfo(ast)
    super.walk(ast)
  }
}
