package kr.ac.kaist.jsaver.js

import kr.ac.kaist.jsaver.js.ast.{ CoalesceExpression, CoalesceExpression0, CoalesceExpressionHead, CoalesceExpressionHead0, CoalesceExpressionHead1, DoWhileStatement, DoWhileStatement0 }
import kr.ac.kaist.jsaver.parser.ESValueParser
import kr.ac.kaist.jsaver.util.Span

// The following parsers were manually written in the original JSAVER distribution.
// The `ParserGenerator` generates parsers for these productions, but they are incorrect.
// This object overrides the generated parsers with the original manual ones.
object Parser extends GeneratedParser {
  // TODO automatically synthesize this part: handling unicodes
  override lazy val IdentifierName: Lexer = IdentifierStart ~ rep(IdentifierPart) ^^ {
    case s ~ ps => ESValueParser.parseIdentifier(ps.foldLeft(s)(_ + _))
  }

  // TODO automatically synthesize this part: left recursion
  override lazy val CoalesceExpression: ESParser[CoalesceExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("CoalesceExpression", args, 3)
    log((
      log((MATCH ~ CoalesceExpressionHead(List(pIn, pYield, pAwait)) <~ t("??")) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => CoalesceExpression0(x0, x1, args, Span()) })("CoalesceExpression0")
    ))("CoalesceExpression")
  })

  // TODO automatically synthesize this part: left recursion
  override lazy val CoalesceExpressionHead: ESParser[CoalesceExpressionHead] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("CoalesceExpressionHead", args, 3)
    log(resolveLR((
      log(MATCH ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => CoalesceExpressionHead1(x0, args, Span()) })("CoalesceExpressionHead1")
    ), (
      log((MATCH <~ t("??")) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => ((x: CoalesceExpressionHead) => CoalesceExpressionHead0(CoalesceExpression0(x, x0, args, Span()), args, Span())) })("CoalesceExpressionHead0")
    )))("CoalesceExpressionHead")
  })

  // TODO automatically synthesize this part: do-while token for automatic semicolon insertion
  override lazy val DoWhileStatement: ESParser[DoWhileStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("DoWhileStatement", args, 3)
    log((
      log((((((MATCH <~ t("do")) ~ Statement(List(pYield, pAwait, pReturn)) <~ t("while")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ doWhileCloseT) <~ t(";")) ^^ { case _ ~ x0 ~ x1 => DoWhileStatement0(x0, x1, args, Span()) })("DoWhileStatement0")
    ))("DoWhileStatement")
  })
}
