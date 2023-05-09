package kr.ac.kaist.jsaver.generator

import kr.ac.kaist.jsaver._
import kr.ac.kaist.jsaver.spec.JsonProtocol._
import kr.ac.kaist.jsaver.spec.NativeHelper._
import kr.ac.kaist.jsaver.spec._
import kr.ac.kaist.jsaver.spec.grammar.Grammar
import kr.ac.kaist.jsaver.util.JvmUseful._
import kr.ac.kaist.jsaver.util.Useful._

case class ModelGenerator(spec: ECMAScript, parser: Boolean) {
  val version = spec.version
  val grammar = spec.grammar

  // generate model/VERSION in resource directory
  dumpSpec(spec, s"$RESOURCE_DIR/$version/generated")

  // generate js/ast/*.scala in source code directory
  ASTGenerator(grammar)

  // generate js/GeneratedParser.scala in source code directory
  if (parser) ParserGenerator(grammar)

  // generate js/ASTWalker.scala in source code directory
  WalkerGenerator(grammar)

  // generate js/ASTDiff.scala in source code directory
  DiffGenerator(grammar)
}
