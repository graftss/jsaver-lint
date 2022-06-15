package kr.ac.kaist.jsaver.analyzer

import kr.ac.kaist.jsaver._
import kr.ac.kaist.jsaver.js.Test262
import kr.ac.kaist.jsaver.util.Useful._
import kr.ac.kaist.jsaver.util.JvmUseful._

class Test262Test extends AnalyzerTest {
  val name: String = "test262Test"

  // offset and stride
  val offset = optional(System.getenv("JSAVER_OFFSET").toInt).getOrElse(0)
  val stride = optional(System.getenv("JSAVER_STRIDE").toInt).getOrElse(1)

  // filename for analyze target names
  val filename = s"$BASE_DIR/tests/analyze-test262"

  // logging
  LOG = true

  // registration
  def init: Unit = check(name, {
    val targets = readFile(filename).split(LINE_SEP).toSet
    val tests = Test262.config.normal.filter(targets contains _.name)
    val sliced = slice(tests, offset, stride)
    test262Test(sliced, "test262")
  })
  init
}
