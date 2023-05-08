package kr.ac.kaist.jsaver.phase

import kr.ac.kaist.jsaver.extractor.ECMAScriptParser
import kr.ac.kaist.jsaver.generator.ParserGenerator
import kr.ac.kaist.jsaver.{ JSAVERConfig, VERSION }
import kr.ac.kaist.jsaver.spec.ECMAScript
import kr.ac.kaist.jsaver.util.{ OptionKind, StrOption }
import kr.ac.kaist.jsaver.util.Useful.time

case class GenerateParserConfig(
  var outPath: Option[String] = None
) extends Config

case object GenerateParser extends Phase[Unit, GenerateParserConfig, Unit] {
  override val name: String = "gen-parser"
  override val help: String = "generate the JS parser as `Parser.scala`"

  override def defaultConfig: GenerateParserConfig = GenerateParserConfig()

  def apply(
    unit: Unit,
    jsaverConfig: JSAVERConfig,
    config: GenerateParserConfig
  ): Unit = {
    val grammar = {
      val version = VERSION
      println(s"version: $version")
      val (g, d) = ECMAScriptParser.parseGrammar(version)
      g
    }

    println("generating parser...")
    ParserGenerator(grammar)

    ()
  }

  override val options: List[(String, OptionKind[GenerateParserConfig], String)] = List(
    ("outPath", StrOption((c, s) => c.outPath = Some(s)),
      "set the git version of ecma262."),
  )
}
