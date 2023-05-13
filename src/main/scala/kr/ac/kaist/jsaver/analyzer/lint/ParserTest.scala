package kr.ac.kaist.jsaver.analyzer.lint

import kr.ac.kaist.jsaver.js
import kr.ac.kaist.jsaver.js.Parser._
import kr.ac.kaist.jsaver.js.ast.{ AST, Declaration, Declaration0, Declaration1, Declaration2, Expression, StatementList, StatementList0, StatementList1, StatementListItem, StatementListItem0, StatementListItem1 }
import kr.ac.kaist.jsaver.util.{ Pos, Span }

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Position

object ParserTest {
  def withSpan[T <: AST](p: LAParser[T]): LAParser[T] = {
    new LAParser(follow => Parser { rawIn =>
      val in = rawIn.asInstanceOf[EPackratReader[Char]]
      val result = p(follow, in)
      result.map(value => {
        value.span.start = Pos(in.pos)
        value.span.end = Pos(result.next.pos)
        value
      })
    }, p.first)
  }

  lazy val Declaration: ESParser[Declaration] = memo(args => {
    val List(pYield, pAwait) = getArgsN("Declaration", args, 2)
    withSpan(MATCH ~ HoistableDeclaration(List(pYield, pAwait, false)) ^^ { case _ ~ x0 => Declaration0(x0, args, Span()) }) |
      withSpan(MATCH ~ ClassDeclaration(List(pYield, pAwait, false)) ^^ { case _ ~ x0 => Declaration1(x0, args, Span()) }) |
      withSpan(MATCH ~ LexicalDeclaration(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => Declaration2(x0, args, Span()) })
  })

  lazy val StatementListItem: ESParser[StatementListItem] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("StatementListItem", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comments) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ c ~ x0 => StatementListItem0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comments) ~ Declaration(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => StatementListItem1(x0, args, Span(rawPreComment = c)) })
    ))
  })

  lazy val StatementList: ESParser[StatementList] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("StatementList", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ StatementListItem(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => StatementList0(x0, args, Span()) })
    ), (
      log(MATCH ~ StatementListItem(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => ((x: StatementList) => StatementList1(x, x0, args, Span())) })("StatementList1")
    )))
  })

  def main(): Unit = {
    val code = "   var x = a + b; \n\nvar y = c + d"

    val p = StatementList(List(false, false, false))
    val expr = js.Parser.parse(p, code)
    println(s"expr: ${expr}")
    println(expr.map(_.toTreeString(0)))
  }
}
