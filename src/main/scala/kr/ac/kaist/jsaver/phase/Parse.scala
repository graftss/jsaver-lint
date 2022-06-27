package kr.ac.kaist.jsaver.phase

import io.circe._, io.circe.syntax._, io.circe.parser._
import kr.ac.kaist.jsaver.BASE_DIR
import kr.ac.kaist.jsaver.error.NotSupported
import kr.ac.kaist.jsaver.js._
import kr.ac.kaist.jsaver.js.ast.Script
import kr.ac.kaist.jsaver.parser.{ MetaParser, MetaData }
import kr.ac.kaist.jsaver.util.JvmUseful._
import kr.ac.kaist.jsaver.util.Useful._
import kr.ac.kaist.jsaver.util._
import kr.ac.kaist.jsaver.{ LINE_SEP, JSAVERConfig }

// Parse phase
case object Parse extends Phase[Unit, ParseConfig, Script] {
  val name = "parse"
  val help = "parses a JavaScript file using the generated parser."

  def apply(
    unit: Unit,
    jsaverConfig: JSAVERConfig,
    config: ParseConfig
  ): Script = {
    val filename = getFirstFilename(jsaverConfig, "parse")
    val ast = parseJS(jsaverConfig.args, config.esparse) match {
      case ast if config.test262 => prependedTest262Harness(filename, ast)
      case ast => ast
    }
    config.jsonFile match {
      case Some(name) =>
        val nf = getPrintWriter(name)
        nf.println(ast.toJson.noSpaces)
        nf.close()
      case None =>
    }
    if (config.pprint) println(ast.prettify.noSpaces)

    ast
  }

  // parse JavaScript files
  def parseJS(list: List[String], esparse: Boolean): Script = list match {
    case List(filename) => parseJS(filename, esparse)
    case _ => mergeStmt(for {
      filename <- list
      script = parseJS(filename, esparse)
      item <- flattenStmt(script)
    } yield item)
  }
  def parseJS(filename: String, esparse: Boolean): Script = {
    if (esparse) {
      val code = parse(executeCmd(s"$BASE_DIR/bin/esparse $filename"))
        .getOrElse(error("invalid AST"))
      Script(code)
    } else {
      Parser.parse(Parser.Script(Nil), fileReader(filename)).get
    }
  }

  // prepend harness.js for Test262
  def prependedTest262Harness(filename: String, script: Script): Script = {
    import Test262._
    val meta = MetaParser(filename)
    val includes = meta.includes
    val includeStmts = includes.foldLeft(basicStmts) {
      case (li, s) => for {
        x <- li
        y <- getInclude(s)
      } yield x ++ y
    } match {
      case Right(l) => l
      case Left(msg) => throw NotSupported(msg)
    }
    val stmts = includeStmts ++ flattenStmt(script)
    mergeStmt(stmts)
  }

  def defaultConfig: ParseConfig = ParseConfig()
  val options: List[PhaseOption[ParseConfig]] = List()
}

// Parse phase config
case class ParseConfig(
  var jsonFile: Option[String] = None,
  var esparse: Boolean = false,
  var test262: Boolean = false,
  var pprint: Boolean = false
) extends Config
