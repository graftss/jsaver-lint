package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.js.ASTWalker
import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.util.Span

// data recorded for each function definition in a program
case class FuncDefInfo(ast: AST, bodyHash: Int, name: Option[String], span: Span) {
  def getName: String = name.getOrElse("[anonymous]")
}

// data recorded for each mutation expression in a program
case class MutationInfo(exprHash: Int, span: Span)

class LintWalker extends ASTWalker {
  // maps hashes of function definition bodies to relevant data about each definition
  var funcDefs: Map[Int, FuncDefInfo] = Map()
  var mutations: Map[Int, MutationInfo] = Map()

  // attempt to parse a `FuncDefInfo` object from an AST node
  def parseFuncDefInfo(ast: AST): Option[FuncDefInfo] = ast match {
    // Script :: ScriptBody
    case Script0(Some(x0), _, span) =>
      Some(FuncDefInfo(ast, x0.hashCode, Some("File"), span))

    // FunctionDeclaration :: "function" BindingIdentifier "(" FormalParameters ")" "{" FunctionBody "}"
    case FunctionDeclaration0(x1, _, x6, _, span) =>
      Some(FuncDefInfo(ast, x6.hashCode, Some(x1.toString), span))

    // FunctionDeclaration :: "function" "(" FormalParameters ")" "{" FunctionBody "}"
    case FunctionDeclaration1(_, x5, _, span) =>
      Some(FuncDefInfo(ast, x5.hashCode, None, span))

    // FunctionExpression :: "function" BindingIdentifier "(" FormalParameters ")" "{" FunctionBody "}"
    case FunctionExpression0(x1, _, x6, _, span) =>
      Some(FuncDefInfo(ast, x6.hashCode, x1.map(_.toString), span))

    // MethodDefinition :: PropertyName "(" UniqueFormalParameters ")" "{" FunctionBody "}"
    case MethodDefinition0(x0, x2, x5, _, span) =>
      Some(FuncDefInfo(ast, x5.hashCode, Some(x0.toString), span))

    case _ => None
  }

  def recordFuncDefInfo(ast: AST): Unit = {
    parseFuncDefInfo(ast).foreach(info => {
      funcDefs += info.bodyHash -> info
    })
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

  override def walk(ast: MethodDefinition): Unit = {
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
