package kr.ac.kaist.jsaver.js

import kr.ac.kaist.jsaver.js.ast._
import kr.ac.kaist.jsaver.ir._
import kr.ac.kaist.jsaver.parser.ESParsers
import kr.ac.kaist.jsaver.util.Span

trait GeneratedParser extends ESParsers {
  lazy val StringNumericLiteral: Lexer = (
    StrWhiteSpace.opt |||
    StrWhiteSpace.opt % StrNumericLiteral % StrWhiteSpace.opt
  )
  lazy val StrWhiteSpace: Lexer = (
    StrWhiteSpaceChar % StrWhiteSpace.opt
  )
  lazy val StrWhiteSpaceChar: Lexer = (
    WhiteSpace |||
    LineTerminator
  )
  lazy val StrNumericLiteral: Lexer = (
    StrDecimalLiteral |||
    NonDecimalIntegerLiteral
  )
  lazy val StrDecimalLiteral: Lexer = (
    StrUnsignedDecimalLiteral |||
    "+" % StrUnsignedDecimalLiteral |||
    "-" % StrUnsignedDecimalLiteral
  )
  lazy val StrUnsignedDecimalLiteral: Lexer = (
    "Infinity" |||
    DecimalDigits % "." % DecimalDigits.opt % ExponentPart.opt |||
    "." % DecimalDigits % ExponentPart.opt |||
    DecimalDigits % ExponentPart.opt
  )
  lazy val SourceCharacter: Lexer = (
    Unicode
  )
  lazy val InputElementDiv: Lexer = (
    WhiteSpace |||
    LineTerminator |||
    Comment |||
    CommonToken |||
    DivPunctuator |||
    RightBracePunctuator
  )
  lazy val InputElementRegExp: Lexer = (
    WhiteSpace |||
    LineTerminator |||
    Comment |||
    CommonToken |||
    RightBracePunctuator |||
    RegularExpressionLiteral
  )
  lazy val InputElementRegExpOrTemplateTail: Lexer = (
    WhiteSpace |||
    LineTerminator |||
    Comment |||
    CommonToken |||
    RegularExpressionLiteral |||
    TemplateSubstitutionTail
  )
  lazy val InputElementTemplateTail: Lexer = (
    WhiteSpace |||
    LineTerminator |||
    Comment |||
    CommonToken |||
    DivPunctuator |||
    TemplateSubstitutionTail
  )
  lazy val MultiLineNotAsteriskChar: Lexer = (
    (SourceCharacter \ ("*"))
  )
  lazy val MultiLineNotForwardSlashOrAsteriskChar: Lexer = (
    (SourceCharacter \ ("/" ||| "*"))
  )
  lazy val CommonToken: Lexer = (
    IdentifierName |||
    Punctuator |||
    NumericLiteral |||
    StringLiteral |||
    Template
  )
  lazy val IdentifierName: Lexer = (
    IdentifierStart |||
    IdentifierName % IdentifierPart
  )
  lazy val IdentifierStart: Lexer = (
    UnicodeIDStart |||
    "$" |||
    "_" |||
    "\\" % UnicodeEscapeSequence
  )
  lazy val IdentifierPart: Lexer = (
    UnicodeIDContinue |||
    "$" |||
    "\\" % UnicodeEscapeSequence |||
    ZWNJ |||
    ZWJ
  )
  lazy val UnicodeIDStart: Lexer = (
    IDStart
  )
  lazy val UnicodeIDContinue: Lexer = (
    IDContinue
  )
  lazy val ReservedWord: Lexer = (
    "await" |||
    "break" |||
    "case" |||
    "catch" |||
    "class" |||
    "const" |||
    "continue" |||
    "debugger" |||
    "default" |||
    "delete" |||
    "do" |||
    "else" |||
    "enum" |||
    "export" |||
    "extends" |||
    "false" |||
    "finally" |||
    "for" |||
    "function" |||
    "if" |||
    "import" |||
    "in" |||
    "instanceof" |||
    "new" |||
    "null" |||
    "return" |||
    "super" |||
    "switch" |||
    "this" |||
    "throw" |||
    "true" |||
    "try" |||
    "typeof" |||
    "var" |||
    "void" |||
    "while" |||
    "with" |||
    "yield"
  )
  lazy val Punctuator: Lexer = (
    OptionalChainingPunctuator |||
    OtherPunctuator
  )
  lazy val OptionalChainingPunctuator: Lexer = (
    "?." % -(DecimalDigit)
  )
  lazy val OtherPunctuator: Lexer = (
    "{" |||
    "(" |||
    ")" |||
    "[" |||
    "]" |||
    "." |||
    "..." |||
    ";" |||
    "," |||
    "<" |||
    ">" |||
    "<=" |||
    ">=" |||
    "==" |||
    "!=" |||
    "===" |||
    "!==" |||
    "+" |||
    "-" |||
    "*" |||
    "%" |||
    "**" |||
    "++" |||
    "--" |||
    "<<" |||
    ">>" |||
    ">>>" |||
    "&" |||
    "|" |||
    "^" |||
    "!" |||
    "~" |||
    "&&" |||
    "||" |||
    "??" |||
    "?" |||
    ":" |||
    "=" |||
    "+=" |||
    "-=" |||
    "*=" |||
    "%=" |||
    "**=" |||
    "<<=" |||
    ">>=" |||
    ">>>=" |||
    "&=" |||
    "|=" |||
    "^=" |||
    "&&=" |||
    "||=" |||
    "??=" |||
    "=>"
  )
  lazy val DivPunctuator: Lexer = (
    "/" |||
    "/="
  )
  lazy val RightBracePunctuator: Lexer = (
    "}"
  )
  lazy val NullLiteral: Lexer = (
    "null"
  )
  lazy val BooleanLiteral: Lexer = (
    "true" |||
    "false"
  )
  lazy val NumericLiteralSeparator: Lexer = (
    "_"
  )
  lazy val NumericLiteral: Lexer = (
    DecimalLiteral |||
    DecimalBigIntegerLiteral |||
    NonDecimalIntegerLiteral |||
    NonDecimalIntegerLiteral % BigIntLiteralSuffix
  )
  lazy val DecimalBigIntegerLiteral: Lexer = (
    "0" % BigIntLiteralSuffix |||
    NonZeroDigit % DecimalDigits.opt % BigIntLiteralSuffix |||
    NonZeroDigit % NumericLiteralSeparator % DecimalDigits % BigIntLiteralSuffix
  )
  lazy val NonDecimalIntegerLiteral: Lexer = (
    BinaryIntegerLiteral |||
    OctalIntegerLiteral |||
    HexIntegerLiteral
  )
  lazy val BigIntLiteralSuffix: Lexer = (
    "n"
  )
  lazy val DecimalLiteral: Lexer = (
    DecimalIntegerLiteral % "." % DecimalDigits.opt % ExponentPart.opt |||
    "." % DecimalDigits % ExponentPart.opt |||
    DecimalIntegerLiteral % ExponentPart.opt
  )
  lazy val DecimalIntegerLiteral: Lexer = (
    "0" |||
    NonZeroDigit |||
    NonZeroDigit % NumericLiteralSeparator.opt % DecimalDigits
  )
  lazy val DecimalDigits: Lexer = (
    DecimalDigit |||
    DecimalDigits % DecimalDigit |||
    DecimalDigits % NumericLiteralSeparator % DecimalDigit
  )
  lazy val DecimalDigit: Lexer = (
    "0" |||
    "1" |||
    "2" |||
    "3" |||
    "4" |||
    "5" |||
    "6" |||
    "7" |||
    "8" |||
    "9"
  )
  lazy val NonZeroDigit: Lexer = (
    "1" |||
    "2" |||
    "3" |||
    "4" |||
    "5" |||
    "6" |||
    "7" |||
    "8" |||
    "9"
  )
  lazy val ExponentPart: Lexer = (
    ExponentIndicator % SignedInteger
  )
  lazy val ExponentIndicator: Lexer = (
    "e" |||
    "E"
  )
  lazy val SignedInteger: Lexer = (
    DecimalDigits |||
    "+" % DecimalDigits |||
    "-" % DecimalDigits
  )
  lazy val BinaryIntegerLiteral: Lexer = (
    "0b" % BinaryDigits |||
    "0B" % BinaryDigits
  )
  lazy val BinaryDigits: Lexer = (
    BinaryDigit |||
    BinaryDigits % BinaryDigit |||
    BinaryDigits % NumericLiteralSeparator % BinaryDigit
  )
  lazy val BinaryDigit: Lexer = (
    "0" |||
    "1"
  )
  lazy val OctalIntegerLiteral: Lexer = (
    "0o" % OctalDigits |||
    "0O" % OctalDigits
  )
  lazy val OctalDigits: Lexer = (
    OctalDigit |||
    OctalDigits % OctalDigit |||
    OctalDigits % NumericLiteralSeparator % OctalDigit
  )
  lazy val OctalDigit: Lexer = (
    "0" |||
    "1" |||
    "2" |||
    "3" |||
    "4" |||
    "5" |||
    "6" |||
    "7"
  )
  lazy val HexIntegerLiteral: Lexer = (
    "0x" % HexDigits |||
    "0X" % HexDigits
  )
  lazy val HexDigits: Lexer = (
    HexDigit |||
    HexDigits % HexDigit |||
    HexDigits % NumericLiteralSeparator % HexDigit
  )
  lazy val HexDigit: Lexer = (
    "0" |||
    "1" |||
    "2" |||
    "3" |||
    "4" |||
    "5" |||
    "6" |||
    "7" |||
    "8" |||
    "9" |||
    "a" |||
    "b" |||
    "c" |||
    "d" |||
    "e" |||
    "f" |||
    "A" |||
    "B" |||
    "C" |||
    "D" |||
    "E" |||
    "F"
  )
  lazy val StringLiteral: Lexer = (
    "\"" % DoubleStringCharacters.opt % "\"" |||
    "'" % SingleStringCharacters.opt % "'"
  )
  lazy val DoubleStringCharacters: Lexer = (
    DoubleStringCharacter % DoubleStringCharacters.opt
  )
  lazy val SingleStringCharacters: Lexer = (
    SingleStringCharacter % SingleStringCharacters.opt
  )
  lazy val DoubleStringCharacter: Lexer = (
    (SourceCharacter \ ("\"" ||| "\\" ||| LineTerminator)) |||
    LS |||
    PS |||
    "\\" % EscapeSequence |||
    LineContinuation
  )
  lazy val SingleStringCharacter: Lexer = (
    (SourceCharacter \ ("'" ||| "\\" ||| LineTerminator)) |||
    LS |||
    PS |||
    "\\" % EscapeSequence |||
    LineContinuation
  )
  lazy val LineContinuation: Lexer = (
    "\\" % LineTerminatorSequence
  )
  lazy val EscapeSequence: Lexer = (
    CharacterEscapeSequence |||
    "0" % -(DecimalDigit) |||
    HexEscapeSequence |||
    UnicodeEscapeSequence
  )
  lazy val CharacterEscapeSequence: Lexer = (
    SingleEscapeCharacter |||
    NonEscapeCharacter
  )
  lazy val SingleEscapeCharacter: Lexer = (
    "'" |||
    "\"" |||
    "\\" |||
    "b" |||
    "f" |||
    "n" |||
    "r" |||
    "t" |||
    "v"
  )
  lazy val NonEscapeCharacter: Lexer = (
    (SourceCharacter \ (EscapeCharacter ||| LineTerminator))
  )
  lazy val EscapeCharacter: Lexer = (
    SingleEscapeCharacter |||
    DecimalDigit |||
    "x" |||
    "u"
  )
  lazy val HexEscapeSequence: Lexer = (
    "x" % HexDigit % HexDigit
  )
  lazy val UnicodeEscapeSequence: Lexer = (
    "u" % Hex4Digits |||
    "u{" % CodePoint % "}"
  )
  lazy val Hex4Digits: Lexer = (
    HexDigit % HexDigit % HexDigit % HexDigit
  )
  lazy val RegularExpressionLiteral: Lexer = (
    "/" % RegularExpressionBody % "/" % RegularExpressionFlags
  )
  lazy val RegularExpressionBody: Lexer = (
    RegularExpressionFirstChar % RegularExpressionChars
  )
  lazy val RegularExpressionChars: Lexer = (
    EMPTY |||
    RegularExpressionChars % RegularExpressionChar
  )
  lazy val RegularExpressionFirstChar: Lexer = (
    (RegularExpressionNonTerminator \ ("*" ||| "\\" ||| "/" ||| "[")) |||
    RegularExpressionBackslashSequence |||
    RegularExpressionClass
  )
  lazy val RegularExpressionChar: Lexer = (
    (RegularExpressionNonTerminator \ ("\\" ||| "/" ||| "[")) |||
    RegularExpressionBackslashSequence |||
    RegularExpressionClass
  )
  lazy val RegularExpressionBackslashSequence: Lexer = (
    "\\" % RegularExpressionNonTerminator
  )
  lazy val RegularExpressionNonTerminator: Lexer = (
    (SourceCharacter \ (LineTerminator))
  )
  lazy val RegularExpressionClass: Lexer = (
    "[" % RegularExpressionClassChars % "]"
  )
  lazy val RegularExpressionClassChars: Lexer = (
    EMPTY |||
    RegularExpressionClassChars % RegularExpressionClassChar
  )
  lazy val RegularExpressionClassChar: Lexer = (
    (RegularExpressionNonTerminator \ ("]" ||| "\\")) |||
    RegularExpressionBackslashSequence
  )
  lazy val RegularExpressionFlags: Lexer = (
    EMPTY |||
    RegularExpressionFlags % IdentifierPart
  )
  lazy val Template: Lexer = (
    NoSubstitutionTemplate |||
    TemplateHead
  )
  lazy val NoSubstitutionTemplate: Lexer = (
    "`" % TemplateCharacters.opt % "`"
  )
  lazy val TemplateHead: Lexer = (
    "`" % TemplateCharacters.opt % "${"
  )
  lazy val TemplateSubstitutionTail: Lexer = (
    TemplateMiddle |||
    TemplateTail
  )
  lazy val TemplateMiddle: Lexer = (
    "}" % TemplateCharacters.opt % "${"
  )
  lazy val TemplateTail: Lexer = (
    "}" % TemplateCharacters.opt % "`"
  )
  lazy val TemplateCharacters: Lexer = (
    TemplateCharacter % TemplateCharacters.opt
  )
  lazy val TemplateCharacter: Lexer = (
    "$" % -("{") |||
    "\\" % EscapeSequence |||
    "\\" % NotEscapeSequence |||
    LineContinuation |||
    LineTerminatorSequence |||
    (SourceCharacter \ ("`" ||| "\\" ||| "$" ||| LineTerminator))
  )
  lazy val NotEscapeSequence: Lexer = (
    "0" % DecimalDigit |||
    (DecimalDigit \ ("0")) |||
    "x" % -(HexDigit) |||
    "x" % HexDigit % -(HexDigit) |||
    "u" % -(HexDigit) % -("{") |||
    "u" % HexDigit % -(HexDigit) |||
    "u" % HexDigit % HexDigit % -(HexDigit) |||
    "u" % HexDigit % HexDigit % HexDigit % -(HexDigit) |||
    "u" % "{" % -(HexDigit) |||
    "u" % "{" % NotCodePoint % -(HexDigit) |||
    "u" % "{" % CodePoint % -(HexDigit) % -("}")
  )
  lazy val NotCodePoint: Lexer = (
    HexDigits
  )
  lazy val CodePoint: Lexer = (
    HexDigits
  )
  lazy val IdentifierReference: ESParser[IdentifierReference] = memo(args => {
    val List(pYield, pAwait) = getArgsN("IdentifierReference", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ Identifier(List()) ^^ { case _ ~ c ~ x0 => IdentifierReference0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((if (!pYield) (MATCH ~ opt(comment) <~ t("yield")) ^^ { case _ ~ c => IdentifierReference1(args, Span(rawPreComment = c)) } else MISMATCH)) |
      withSpan((if (!pAwait) (MATCH ~ opt(comment) <~ t("await")) ^^ { case _ ~ c => IdentifierReference2(args, Span(rawPreComment = c)) } else MISMATCH))
    ))
  })
  lazy val BindingIdentifier: ESParser[BindingIdentifier] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingIdentifier", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ Identifier(List()) ^^ { case _ ~ c ~ x0 => BindingIdentifier0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("yield")) ^^ { case _ ~ c => BindingIdentifier1(args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("await")) ^^ { case _ ~ c => BindingIdentifier2(args, Span(rawPreComment = c)) })
    ))
  })
  lazy val LabelIdentifier: ESParser[LabelIdentifier] = memo(args => {
    val List(pYield, pAwait) = getArgsN("LabelIdentifier", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ Identifier(List()) ^^ { case _ ~ c ~ x0 => LabelIdentifier0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((if (!pYield) (MATCH ~ opt(comment) <~ t("yield")) ^^ { case _ ~ c => LabelIdentifier1(args, Span(rawPreComment = c)) } else MISMATCH)) |
      withSpan((if (!pAwait) (MATCH ~ opt(comment) <~ t("await")) ^^ { case _ ~ c => LabelIdentifier2(args, Span(rawPreComment = c)) } else MISMATCH))
    ))
  })
  lazy val Identifier: ESParser[Identifier] = memo(args => {
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ nt("""(IdentifierName \ (ReservedWord))""", (IdentifierName \ (ReservedWord))) ^^ { case _ ~ c ~ x0 => Identifier0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val PrimaryExpression: ESParser[PrimaryExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("PrimaryExpression", args, 2)
    withSpan((
      withSpan((MATCH ~ opt(comment) <~ t("this")) ^^ { case _ ~ c => PrimaryExpression0(args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ IdentifierReference(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => PrimaryExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ Literal(List()) ^^ { case _ ~ c ~ x0 => PrimaryExpression2(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ ArrayLiteral(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => PrimaryExpression3(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ ObjectLiteral(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => PrimaryExpression4(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ FunctionExpression(List()) ^^ { case _ ~ c ~ x0 => PrimaryExpression5(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ ClassExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => PrimaryExpression6(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ GeneratorExpression(List()) ^^ { case _ ~ c ~ x0 => PrimaryExpression7(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ AsyncFunctionExpression(List()) ^^ { case _ ~ c ~ x0 => PrimaryExpression8(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ AsyncGeneratorExpression(List()) ^^ { case _ ~ c ~ x0 => PrimaryExpression9(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ nt("RegularExpressionLiteral", RegularExpressionLiteral) ^^ { case _ ~ c ~ x0 => PrimaryExpression10(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ TemplateLiteral(List(pYield, pAwait, false)) ^^ { case _ ~ c ~ x0 => PrimaryExpression11(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ CoverParenthesizedExpressionAndArrowParameterList(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => PrimaryExpression12(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val CoverParenthesizedExpressionAndArrowParameterList: ESParser[CoverParenthesizedExpressionAndArrowParameterList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CoverParenthesizedExpressionAndArrowParameterList", args, 2)
    withSpan((
      withSpan(((MATCH ~ opt(comment) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 => CoverParenthesizedExpressionAndArrowParameterList0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((((MATCH ~ opt(comment) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(",")) <~ t(")")) ^^ { case _ ~ c ~ x0 => CoverParenthesizedExpressionAndArrowParameterList1(x0, args, Span(rawPreComment = c)) }) |
      withSpan(((MATCH ~ opt(comment) <~ t("(")) <~ t(")")) ^^ { case _ ~ c => CoverParenthesizedExpressionAndArrowParameterList2(args, Span(rawPreComment = c)) }) |
      withSpan((((MATCH ~ opt(comment) <~ t("(")) <~ t("...")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 => CoverParenthesizedExpressionAndArrowParameterList3(x0, args, Span(rawPreComment = c)) }) |
      withSpan((((MATCH ~ opt(comment) <~ t("(")) <~ t("...")) ~ BindingPattern(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 => CoverParenthesizedExpressionAndArrowParameterList4(x0, args, Span(rawPreComment = c)) }) |
      withSpan(((((MATCH ~ opt(comment) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(",")) <~ t("...")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 ~ x1 => CoverParenthesizedExpressionAndArrowParameterList5(x0, x1, args, Span(rawPreComment = c)) }) |
      withSpan(((((MATCH ~ opt(comment) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(",")) <~ t("...")) ~ BindingPattern(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 ~ x1 => CoverParenthesizedExpressionAndArrowParameterList6(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val ParenthesizedExpression: ESParser[ParenthesizedExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ParenthesizedExpression", args, 2)
    withSpan((
      withSpan(((MATCH ~ opt(comment) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ^^ { case _ ~ c ~ x0 => ParenthesizedExpression0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val Literal: ESParser[Literal] = memo(args => {
    withSpan((
      withSpan(MATCH ~ nt("NullLiteral", NullLiteral) ^^ { case _ ~ x0 => Literal0(x0, args, Span()) }) |
      withSpan(MATCH ~ nt("BooleanLiteral", BooleanLiteral) ^^ { case _ ~ x0 => Literal1(x0, args, Span()) }) |
      withSpan(MATCH ~ nt("NumericLiteral", NumericLiteral) ^^ { case _ ~ x0 => Literal2(x0, args, Span()) }) |
      withSpan(MATCH ~ nt("StringLiteral", StringLiteral) ^^ { case _ ~ x0 => Literal3(x0, args, Span()) })
    ))
  })
  lazy val ArrayLiteral: ESParser[ArrayLiteral] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArrayLiteral", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("[")) ~ opt(Elision(List())) <~ t("]")) ^^ { case _ ~ x0 => ArrayLiteral0(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("[")) ~ ElementList(List(pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => ArrayLiteral1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("[")) ~ ElementList(List(pYield, pAwait)) <~ t(",")) ~ opt(Elision(List())) <~ t("]")) ^^ { case _ ~ x0 ~ x1 => ArrayLiteral2(x0, x1, args, Span()) })
    ))
  })
  lazy val ElementList: ESParser[ElementList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ElementList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(Elision(List())) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ElementList0(x0, x1, args, Span()) }) |
      withSpan(MATCH ~ opt(Elision(List())) ~ SpreadElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ElementList1(x0, x1, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ opt(Elision(List())) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ((x: ElementList) => ElementList2(x, x0, x1, args, Span())) })("ElementList2") |
      log((MATCH <~ t(",")) ~ opt(Elision(List())) ~ SpreadElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ((x: ElementList) => ElementList3(x, x0, x1, args, Span())) })("ElementList3")
    )))
  })
  lazy val Elision: ESParser[Elision] = memo(args => {
    withSpan(resolveLR((
      withSpan((MATCH <~ t(",")) ^^ { case _ => Elision0(args, Span()) })
    ), (
      log((MATCH <~ t(",")) ^^ { case _ => ((x: Elision) => Elision1(x, args, Span())) })("Elision1")
    )))
  })
  lazy val SpreadElement: ESParser[SpreadElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("SpreadElement", args, 2)
    withSpan((
      withSpan((MATCH <~ t("...")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => SpreadElement0(x0, args, Span()) })
    ))
  })
  lazy val ObjectLiteral: ESParser[ObjectLiteral] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ObjectLiteral", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("{")) <~ t("}")) ^^ { case _ => ObjectLiteral0(args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ PropertyDefinitionList(List(pYield, pAwait)) <~ t("}")) ^^ { case _ ~ x0 => ObjectLiteral1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("{")) ~ PropertyDefinitionList(List(pYield, pAwait)) <~ t(",")) <~ t("}")) ^^ { case _ ~ x0 => ObjectLiteral2(x0, args, Span()) })
    ))
  })
  lazy val PropertyDefinitionList: ESParser[PropertyDefinitionList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("PropertyDefinitionList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ PropertyDefinition(List(pYield, pAwait)) ^^ { case _ ~ x0 => PropertyDefinitionList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ PropertyDefinition(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: PropertyDefinitionList) => PropertyDefinitionList1(x, x0, args, Span())) })("PropertyDefinitionList1")
    )))
  })
  lazy val PropertyDefinition: ESParser[PropertyDefinition] = memo(args => {
    val List(pYield, pAwait) = getArgsN("PropertyDefinition", args, 2)
    withSpan((
      withSpan(MATCH ~ IdentifierReference(List(pYield, pAwait)) ^^ { case _ ~ x0 => PropertyDefinition0(x0, args, Span()) }) |
      withSpan(MATCH ~ CoverInitializedName(List(pYield, pAwait)) ^^ { case _ ~ x0 => PropertyDefinition1(x0, args, Span()) }) |
      withSpan((MATCH ~ PropertyName(List(pYield, pAwait)) <~ t(":")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => PropertyDefinition2(x0, x1, args, Span()) }) |
      withSpan(MATCH ~ MethodDefinition(List(pYield, pAwait)) ^^ { case _ ~ x0 => PropertyDefinition3(x0, args, Span()) }) |
      withSpan((MATCH <~ t("...")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => PropertyDefinition4(x0, args, Span()) })
    ))
  })
  lazy val PropertyName: ESParser[PropertyName] = memo(args => {
    val List(pYield, pAwait) = getArgsN("PropertyName", args, 2)
    withSpan((
      withSpan(MATCH ~ LiteralPropertyName(List()) ^^ { case _ ~ x0 => PropertyName0(x0, args, Span()) }) |
      withSpan(MATCH ~ ComputedPropertyName(List(pYield, pAwait)) ^^ { case _ ~ x0 => PropertyName1(x0, args, Span()) })
    ))
  })
  lazy val LiteralPropertyName: ESParser[LiteralPropertyName] = memo(args => {
    withSpan((
      withSpan(MATCH ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => LiteralPropertyName0(x0, args, Span()) }) |
      withSpan(MATCH ~ nt("StringLiteral", StringLiteral) ^^ { case _ ~ x0 => LiteralPropertyName1(x0, args, Span()) }) |
      withSpan(MATCH ~ nt("NumericLiteral", NumericLiteral) ^^ { case _ ~ x0 => LiteralPropertyName2(x0, args, Span()) })
    ))
  })
  lazy val ComputedPropertyName: ESParser[ComputedPropertyName] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ComputedPropertyName", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("[")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => ComputedPropertyName0(x0, args, Span()) })
    ))
  })
  lazy val CoverInitializedName: ESParser[CoverInitializedName] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CoverInitializedName", args, 2)
    withSpan((
      withSpan(MATCH ~ IdentifierReference(List(pYield, pAwait)) ~ Initializer(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => CoverInitializedName0(x0, x1, args, Span()) })
    ))
  })
  lazy val Initializer: ESParser[Initializer] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("Initializer", args, 3)
    withSpan((
      withSpan((MATCH <~ t("=")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => Initializer0(x0, args, Span()) })
    ))
  })
  lazy val TemplateLiteral: ESParser[TemplateLiteral] = memo(args => {
    val List(pYield, pAwait, pTagged) = getArgsN("TemplateLiteral", args, 3)
    withSpan((
      withSpan(MATCH ~ nt("NoSubstitutionTemplate", NoSubstitutionTemplate) ^^ { case _ ~ x0 => TemplateLiteral0(x0, args, Span()) }) |
      withSpan(MATCH ~ SubstitutionTemplate(List(pYield, pAwait, pTagged)) ^^ { case _ ~ x0 => TemplateLiteral1(x0, args, Span()) })
    ))
  })
  lazy val SubstitutionTemplate: ESParser[SubstitutionTemplate] = memo(args => {
    val List(pYield, pAwait, pTagged) = getArgsN("SubstitutionTemplate", args, 3)
    withSpan((
      withSpan(MATCH ~ nt("TemplateHead", TemplateHead) ~ Expression(List(true, pYield, pAwait)) ~ TemplateSpans(List(pYield, pAwait, pTagged)) ^^ { case _ ~ x0 ~ x1 ~ x2 => SubstitutionTemplate0(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val TemplateSpans: ESParser[TemplateSpans] = memo(args => {
    val List(pYield, pAwait, pTagged) = getArgsN("TemplateSpans", args, 3)
    withSpan((
      withSpan(MATCH ~ nt("TemplateTail", TemplateTail) ^^ { case _ ~ x0 => TemplateSpans0(x0, args, Span()) }) |
      withSpan(MATCH ~ TemplateMiddleList(List(pYield, pAwait, pTagged)) ~ nt("TemplateTail", TemplateTail) ^^ { case _ ~ x0 ~ x1 => TemplateSpans1(x0, x1, args, Span()) })
    ))
  })
  lazy val TemplateMiddleList: ESParser[TemplateMiddleList] = memo(args => {
    val List(pYield, pAwait, pTagged) = getArgsN("TemplateMiddleList", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ nt("TemplateMiddle", TemplateMiddle) ~ Expression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => TemplateMiddleList0(x0, x1, args, Span()) })
    ), (
      log(MATCH ~ nt("TemplateMiddle", TemplateMiddle) ~ Expression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ((x: TemplateMiddleList) => TemplateMiddleList1(x, x0, x1, args, Span())) })("TemplateMiddleList1")
    )))
  })
  lazy val MemberExpression: ESParser[MemberExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("MemberExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ PrimaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => MemberExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ SuperProperty(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => MemberExpression4(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ MetaProperty(List()) ^^ { case _ ~ c ~ x0 => MemberExpression5(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("new")) ~ MemberExpression(List(pYield, pAwait)) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => MemberExpression6(x0, x1, args, Span(rawPreComment = c)) })
    ), (
      log(((MATCH ~ opt(comment) <~ t("[")) ~ Expression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ c ~ x0 => ((x: MemberExpression) => MemberExpression1(x, x0, args, Span(rawPreComment = c))) })("MemberExpression1") |
      log((MATCH ~ opt(comment) <~ t(".")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ c ~ x0 => ((x: MemberExpression) => MemberExpression2(x, x0, args, Span(rawPreComment = c))) })("MemberExpression2") |
      log(MATCH ~ opt(comment) ~ TemplateLiteral(List(pYield, pAwait, true)) ^^ { case _ ~ c ~ x0 => ((x: MemberExpression) => MemberExpression3(x, x0, args, Span(rawPreComment = c))) })("MemberExpression3")
    )))
  })
  lazy val SuperProperty: ESParser[SuperProperty] = memo(args => {
    val List(pYield, pAwait) = getArgsN("SuperProperty", args, 2)
    withSpan((
      withSpan((((MATCH <~ t("super")) <~ t("[")) ~ Expression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => SuperProperty0(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("super")) <~ t(".")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => SuperProperty1(x0, args, Span()) })
    ))
  })
  lazy val MetaProperty: ESParser[MetaProperty] = memo(args => {
    withSpan((
      withSpan(MATCH ~ NewTarget(List()) ^^ { case _ ~ x0 => MetaProperty0(x0, args, Span()) }) |
      withSpan(MATCH ~ ImportMeta(List()) ^^ { case _ ~ x0 => MetaProperty1(x0, args, Span()) })
    ))
  })
  lazy val NewTarget: ESParser[NewTarget] = memo(args => {
    withSpan((
      withSpan((((MATCH <~ t("new")) <~ t(".")) <~ t("target")) ^^ { case _ => NewTarget0(args, Span()) })
    ))
  })
  lazy val ImportMeta: ESParser[ImportMeta] = memo(args => {
    withSpan((
      withSpan((((MATCH <~ t("import")) <~ t(".")) <~ t("meta")) ^^ { case _ => ImportMeta0(args, Span()) })
    ))
  })
  lazy val NewExpression: ESParser[NewExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("NewExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ MemberExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => NewExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("new")) ~ NewExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => NewExpression1(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val CallExpression: ESParser[CallExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CallExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ CoverCallExpressionAndAsyncArrowHead(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => CallExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ SuperCall(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => CallExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ ImportCall(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => CallExpression2(x0, args, Span(rawPreComment = c)) })
    ), (
      log(MATCH ~ opt(comment) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: CallExpression) => CallExpression3(x, x0, args, Span(rawPreComment = c))) })("CallExpression3") |
      log(((MATCH ~ opt(comment) <~ t("[")) ~ Expression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ c ~ x0 => ((x: CallExpression) => CallExpression4(x, x0, args, Span(rawPreComment = c))) })("CallExpression4") |
      log((MATCH ~ opt(comment) <~ t(".")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ c ~ x0 => ((x: CallExpression) => CallExpression5(x, x0, args, Span(rawPreComment = c))) })("CallExpression5") |
      log(MATCH ~ opt(comment) ~ TemplateLiteral(List(pYield, pAwait, true)) ^^ { case _ ~ c ~ x0 => ((x: CallExpression) => CallExpression6(x, x0, args, Span(rawPreComment = c))) })("CallExpression6")
    )))
  })
  lazy val SuperCall: ESParser[SuperCall] = memo(args => {
    val List(pYield, pAwait) = getArgsN("SuperCall", args, 2)
    withSpan((
      withSpan((MATCH <~ t("super")) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ x0 => SuperCall0(x0, args, Span()) })
    ))
  })
  lazy val ImportCall: ESParser[ImportCall] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ImportCall", args, 2)
    withSpan((
      withSpan((((MATCH <~ t("import")) <~ t("(")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ^^ { case _ ~ x0 => ImportCall0(x0, args, Span()) })
    ))
  })
  lazy val Arguments: ESParser[Arguments] = memo(args => {
    val List(pYield, pAwait) = getArgsN("Arguments", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("(")) <~ t(")")) ^^ { case _ => Arguments0(args, Span()) }) |
      withSpan(((MATCH <~ t("(")) ~ ArgumentList(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ x0 => Arguments1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("(")) ~ ArgumentList(List(pYield, pAwait)) <~ t(",")) <~ t(")")) ^^ { case _ ~ x0 => Arguments2(x0, args, Span()) })
    ))
  })
  lazy val ArgumentList: ESParser[ArgumentList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArgumentList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => ArgumentList0(x0, args, Span()) }) |
      withSpan((MATCH <~ t("...")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => ArgumentList1(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => ((x: ArgumentList) => ArgumentList2(x, x0, args, Span())) })("ArgumentList2") |
      log(((MATCH <~ t(",")) <~ t("...")) ~ AssignmentExpression(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => ((x: ArgumentList) => ArgumentList3(x, x0, args, Span())) })("ArgumentList3")
    )))
  })
  lazy val OptionalExpression: ESParser[OptionalExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("OptionalExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ MemberExpression(List(pYield, pAwait)) ~ OptionalChain(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => OptionalExpression0(x0, x1, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ CallExpression(List(pYield, pAwait)) ~ OptionalChain(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => OptionalExpression1(x0, x1, args, Span(rawPreComment = c)) })
    ), (
      log(MATCH ~ opt(comment) ~ OptionalChain(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: OptionalExpression) => OptionalExpression2(x, x0, args, Span(rawPreComment = c))) })("OptionalExpression2")
    )))
  })
  lazy val OptionalChain: ESParser[OptionalChain] = memo(args => {
    val List(pYield, pAwait) = getArgsN("OptionalChain", args, 2)
    withSpan(resolveLR((
      withSpan((MATCH <~ t("?.")) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ x0 => OptionalChain0(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("?.")) <~ t("[")) ~ Expression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => OptionalChain1(x0, args, Span()) }) |
      withSpan((MATCH <~ t("?.")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => OptionalChain2(x0, args, Span()) }) |
      withSpan((MATCH <~ t("?.")) ~ TemplateLiteral(List(pYield, pAwait, true)) ^^ { case _ ~ x0 => OptionalChain3(x0, args, Span()) })
    ), (
      log(MATCH ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: OptionalChain) => OptionalChain4(x, x0, args, Span())) })("OptionalChain4") |
      log(((MATCH <~ t("[")) ~ Expression(List(true, pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => ((x: OptionalChain) => OptionalChain5(x, x0, args, Span())) })("OptionalChain5") |
      log((MATCH <~ t(".")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => ((x: OptionalChain) => OptionalChain6(x, x0, args, Span())) })("OptionalChain6") |
      log(MATCH ~ TemplateLiteral(List(pYield, pAwait, true)) ^^ { case _ ~ x0 => ((x: OptionalChain) => OptionalChain7(x, x0, args, Span())) })("OptionalChain7")
    )))
  })
  lazy val LeftHandSideExpression: ESParser[LeftHandSideExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("LeftHandSideExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ NewExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => LeftHandSideExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ CallExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => LeftHandSideExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ OptionalExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => LeftHandSideExpression2(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val CallMemberExpression: ESParser[CallMemberExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CallMemberExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ MemberExpression(List(pYield, pAwait)) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => CallMemberExpression0(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val UpdateExpression: ESParser[UpdateExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("UpdateExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UpdateExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ NoLineTerminator) <~ t("++")) ^^ { case _ ~ c ~ x0 => UpdateExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan(((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ NoLineTerminator) <~ t("--")) ^^ { case _ ~ c ~ x0 => UpdateExpression2(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("++")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UpdateExpression3(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("--")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UpdateExpression4(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val UnaryExpression: ESParser[UnaryExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("UnaryExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ UpdateExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("delete")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("void")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression2(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("typeof")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression3(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("+")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression4(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("-")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression5(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("~")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression6(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) <~ t("!")) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => UnaryExpression7(x0, args, Span(rawPreComment = c)) }) |
      withSpan((if (pAwait) MATCH ~ opt(comment) ~ AwaitExpression(List(pYield)) ^^ { case _ ~ c ~ x0 => UnaryExpression8(x0, args, Span(rawPreComment = c)) } else MISMATCH))
    ))
  })
  lazy val ExponentiationExpression: ESParser[ExponentiationExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ExponentiationExpression", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ UnaryExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ExponentiationExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) ~ UpdateExpression(List(pYield, pAwait)) <~ t("**")) ~ ExponentiationExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => ExponentiationExpression1(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val MultiplicativeExpression: ESParser[MultiplicativeExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("MultiplicativeExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ ExponentiationExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => MultiplicativeExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log(MATCH ~ opt(comment) ~ MultiplicativeOperator(List()) ~ ExponentiationExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => ((x: MultiplicativeExpression) => MultiplicativeExpression1(x, x0, x1, args, Span(rawPreComment = c))) })("MultiplicativeExpression1")
    )))
  })
  lazy val MultiplicativeOperator: ESParser[MultiplicativeOperator] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t("*")) ^^ { case _ => MultiplicativeOperator0(args, Span()) }) |
      withSpan((MATCH <~ t("/")) ^^ { case _ => MultiplicativeOperator1(args, Span()) }) |
      withSpan((MATCH <~ t("%")) ^^ { case _ => MultiplicativeOperator2(args, Span()) })
    ))
  })
  lazy val AdditiveExpression: ESParser[AdditiveExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AdditiveExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ MultiplicativeExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => AdditiveExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("+")) ~ MultiplicativeExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: AdditiveExpression) => AdditiveExpression1(x, x0, args, Span(rawPreComment = c))) })("AdditiveExpression1") |
      log((MATCH ~ opt(comment) <~ t("-")) ~ MultiplicativeExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: AdditiveExpression) => AdditiveExpression2(x, x0, args, Span(rawPreComment = c))) })("AdditiveExpression2")
    )))
  })
  lazy val ShiftExpression: ESParser[ShiftExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ShiftExpression", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ AdditiveExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ShiftExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("<<")) ~ AdditiveExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: ShiftExpression) => ShiftExpression1(x, x0, args, Span(rawPreComment = c))) })("ShiftExpression1") |
      log((MATCH ~ opt(comment) <~ t(">>")) ~ AdditiveExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: ShiftExpression) => ShiftExpression2(x, x0, args, Span(rawPreComment = c))) })("ShiftExpression2") |
      log((MATCH ~ opt(comment) <~ t(">>>")) ~ AdditiveExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: ShiftExpression) => ShiftExpression3(x, x0, args, Span(rawPreComment = c))) })("ShiftExpression3")
    )))
  })
  lazy val RelationalExpression: ESParser[RelationalExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("RelationalExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => RelationalExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("<")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression1(x, x0, args, Span(rawPreComment = c))) })("RelationalExpression1") |
      log((MATCH ~ opt(comment) <~ t(">")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression2(x, x0, args, Span(rawPreComment = c))) })("RelationalExpression2") |
      log((MATCH ~ opt(comment) <~ t("<=")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression3(x, x0, args, Span(rawPreComment = c))) })("RelationalExpression3") |
      log((MATCH ~ opt(comment) <~ t(">=")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression4(x, x0, args, Span(rawPreComment = c))) })("RelationalExpression4") |
      log((MATCH ~ opt(comment) <~ t("instanceof")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression5(x, x0, args, Span(rawPreComment = c))) })("RelationalExpression5") |
      log((if (pIn) (MATCH ~ opt(comment) <~ t("in")) ~ ShiftExpression(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: RelationalExpression) => RelationalExpression6(x, x0, args, Span(rawPreComment = c))) } else MISMATCH))("RelationalExpression6")
    )))
  })
  lazy val EqualityExpression: ESParser[EqualityExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("EqualityExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ RelationalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => EqualityExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("==")) ~ RelationalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: EqualityExpression) => EqualityExpression1(x, x0, args, Span(rawPreComment = c))) })("EqualityExpression1") |
      log((MATCH ~ opt(comment) <~ t("!=")) ~ RelationalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: EqualityExpression) => EqualityExpression2(x, x0, args, Span(rawPreComment = c))) })("EqualityExpression2") |
      log((MATCH ~ opt(comment) <~ t("===")) ~ RelationalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: EqualityExpression) => EqualityExpression3(x, x0, args, Span(rawPreComment = c))) })("EqualityExpression3") |
      log((MATCH ~ opt(comment) <~ t("!==")) ~ RelationalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: EqualityExpression) => EqualityExpression4(x, x0, args, Span(rawPreComment = c))) })("EqualityExpression4")
    )))
  })
  lazy val BitwiseANDExpression: ESParser[BitwiseANDExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("BitwiseANDExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ EqualityExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => BitwiseANDExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("&")) ~ EqualityExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: BitwiseANDExpression) => BitwiseANDExpression1(x, x0, args, Span(rawPreComment = c))) })("BitwiseANDExpression1")
    )))
  })
  lazy val BitwiseXORExpression: ESParser[BitwiseXORExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("BitwiseXORExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ BitwiseANDExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => BitwiseXORExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("^")) ~ BitwiseANDExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: BitwiseXORExpression) => BitwiseXORExpression1(x, x0, args, Span(rawPreComment = c))) })("BitwiseXORExpression1")
    )))
  })
  lazy val BitwiseORExpression: ESParser[BitwiseORExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("BitwiseORExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ BitwiseXORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => BitwiseORExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("|")) ~ BitwiseXORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: BitwiseORExpression) => BitwiseORExpression1(x, x0, args, Span(rawPreComment = c))) })("BitwiseORExpression1")
    )))
  })
  lazy val LogicalANDExpression: ESParser[LogicalANDExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("LogicalANDExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => LogicalANDExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("&&")) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: LogicalANDExpression) => LogicalANDExpression1(x, x0, args, Span(rawPreComment = c))) })("LogicalANDExpression1")
    )))
  })
  lazy val LogicalORExpression: ESParser[LogicalORExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("LogicalORExpression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ LogicalANDExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => LogicalORExpression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t("||")) ~ LogicalANDExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: LogicalORExpression) => LogicalORExpression1(x, x0, args, Span(rawPreComment = c))) })("LogicalORExpression1")
    )))
  })
  lazy val CoalesceExpression: ESParser[CoalesceExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("CoalesceExpression", args, 3)
    withSpan((
      withSpan((MATCH ~ opt(comment) ~ CoalesceExpressionHead(List(pIn, pYield, pAwait)) <~ t("??")) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => CoalesceExpression0(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val CoalesceExpressionHead: ESParser[CoalesceExpressionHead] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("CoalesceExpressionHead", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ CoalesceExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => CoalesceExpressionHead0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ BitwiseORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => CoalesceExpressionHead1(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val ShortCircuitExpression: ESParser[ShortCircuitExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("ShortCircuitExpression", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ LogicalORExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ShortCircuitExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ CoalesceExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ShortCircuitExpression1(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val ConditionalExpression: ESParser[ConditionalExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("ConditionalExpression", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ ShortCircuitExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ConditionalExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(((MATCH ~ opt(comment) ~ ShortCircuitExpression(List(pIn, pYield, pAwait)) <~ t("?")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(":")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => ConditionalExpression1(x0, x1, x2, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AssignmentExpression: ESParser[AssignmentExpression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("AssignmentExpression", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ ConditionalExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => AssignmentExpression0(x0, args, Span(rawPreComment = c)) }) |
      withSpan((if (pYield) MATCH ~ opt(comment) ~ YieldExpression(List(pIn, pAwait)) ^^ { case _ ~ c ~ x0 => AssignmentExpression1(x0, args, Span(rawPreComment = c)) } else MISMATCH)) |
      withSpan(MATCH ~ opt(comment) ~ ArrowFunction(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => AssignmentExpression2(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ AsyncArrowFunction(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => AssignmentExpression3(x0, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("=")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => AssignmentExpression4(x0, x1, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) ~ AssignmentOperator(List()) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => AssignmentExpression5(x0, x1, x2, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("&&=")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => AssignmentExpression6(x0, x1, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("||=")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => AssignmentExpression7(x0, x1, args, Span(rawPreComment = c)) }) |
      withSpan((MATCH ~ opt(comment) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("??=")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => AssignmentExpression8(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AssignmentOperator: ESParser[AssignmentOperator] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t("*=")) ^^ { case _ => AssignmentOperator0(args, Span()) }) |
      withSpan((MATCH <~ t("/=")) ^^ { case _ => AssignmentOperator1(args, Span()) }) |
      withSpan((MATCH <~ t("%=")) ^^ { case _ => AssignmentOperator2(args, Span()) }) |
      withSpan((MATCH <~ t("+=")) ^^ { case _ => AssignmentOperator3(args, Span()) }) |
      withSpan((MATCH <~ t("-=")) ^^ { case _ => AssignmentOperator4(args, Span()) }) |
      withSpan((MATCH <~ t("<<=")) ^^ { case _ => AssignmentOperator5(args, Span()) }) |
      withSpan((MATCH <~ t(">>=")) ^^ { case _ => AssignmentOperator6(args, Span()) }) |
      withSpan((MATCH <~ t(">>>=")) ^^ { case _ => AssignmentOperator7(args, Span()) }) |
      withSpan((MATCH <~ t("&=")) ^^ { case _ => AssignmentOperator8(args, Span()) }) |
      withSpan((MATCH <~ t("^=")) ^^ { case _ => AssignmentOperator9(args, Span()) }) |
      withSpan((MATCH <~ t("|=")) ^^ { case _ => AssignmentOperator10(args, Span()) }) |
      withSpan((MATCH <~ t("**=")) ^^ { case _ => AssignmentOperator11(args, Span()) })
    ))
  })
  lazy val AssignmentPattern: ESParser[AssignmentPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentPattern", args, 2)
    withSpan((
      withSpan(MATCH ~ ObjectAssignmentPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentPattern0(x0, args, Span()) }) |
      withSpan(MATCH ~ ArrayAssignmentPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentPattern1(x0, args, Span()) })
    ))
  })
  lazy val ObjectAssignmentPattern: ESParser[ObjectAssignmentPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ObjectAssignmentPattern", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("{")) <~ t("}")) ^^ { case _ => ObjectAssignmentPattern0(args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ AssignmentRestProperty(List(pYield, pAwait)) <~ t("}")) ^^ { case _ ~ x0 => ObjectAssignmentPattern1(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ AssignmentPropertyList(List(pYield, pAwait)) <~ t("}")) ^^ { case _ ~ x0 => ObjectAssignmentPattern2(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("{")) ~ AssignmentPropertyList(List(pYield, pAwait)) <~ t(",")) ~ opt(AssignmentRestProperty(List(pYield, pAwait))) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => ObjectAssignmentPattern3(x0, x1, args, Span()) })
    ))
  })
  lazy val ArrayAssignmentPattern: ESParser[ArrayAssignmentPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArrayAssignmentPattern", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("[")) ~ opt(Elision(List())) ~ opt(AssignmentRestElement(List(pYield, pAwait))) <~ t("]")) ^^ { case _ ~ x0 ~ x1 => ArrayAssignmentPattern0(x0, x1, args, Span()) }) |
      withSpan(((MATCH <~ t("[")) ~ AssignmentElementList(List(pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => ArrayAssignmentPattern1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("[")) ~ AssignmentElementList(List(pYield, pAwait)) <~ t(",")) ~ opt(Elision(List())) ~ opt(AssignmentRestElement(List(pYield, pAwait))) <~ t("]")) ^^ { case _ ~ x0 ~ x1 ~ x2 => ArrayAssignmentPattern2(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val AssignmentRestProperty: ESParser[AssignmentRestProperty] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentRestProperty", args, 2)
    withSpan((
      withSpan((MATCH <~ t("...")) ~ DestructuringAssignmentTarget(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentRestProperty0(x0, args, Span()) })
    ))
  })
  lazy val AssignmentPropertyList: ESParser[AssignmentPropertyList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentPropertyList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ AssignmentProperty(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentPropertyList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ AssignmentProperty(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: AssignmentPropertyList) => AssignmentPropertyList1(x, x0, args, Span())) })("AssignmentPropertyList1")
    )))
  })
  lazy val AssignmentElementList: ESParser[AssignmentElementList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentElementList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ AssignmentElisionElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentElementList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ AssignmentElisionElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: AssignmentElementList) => AssignmentElementList1(x, x0, args, Span())) })("AssignmentElementList1")
    )))
  })
  lazy val AssignmentElisionElement: ESParser[AssignmentElisionElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentElisionElement", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(Elision(List())) ~ AssignmentElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => AssignmentElisionElement0(x0, x1, args, Span()) })
    ))
  })
  lazy val AssignmentProperty: ESParser[AssignmentProperty] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentProperty", args, 2)
    withSpan((
      withSpan(MATCH ~ IdentifierReference(List(pYield, pAwait)) ~ opt(Initializer(List(true, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => AssignmentProperty0(x0, x1, args, Span()) }) |
      withSpan((MATCH ~ PropertyName(List(pYield, pAwait)) <~ t(":")) ~ AssignmentElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => AssignmentProperty1(x0, x1, args, Span()) })
    ))
  })
  lazy val AssignmentElement: ESParser[AssignmentElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentElement", args, 2)
    withSpan((
      withSpan(MATCH ~ DestructuringAssignmentTarget(List(pYield, pAwait)) ~ opt(Initializer(List(true, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => AssignmentElement0(x0, x1, args, Span()) })
    ))
  })
  lazy val AssignmentRestElement: ESParser[AssignmentRestElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AssignmentRestElement", args, 2)
    withSpan((
      withSpan((MATCH <~ t("...")) ~ DestructuringAssignmentTarget(List(pYield, pAwait)) ^^ { case _ ~ x0 => AssignmentRestElement0(x0, args, Span()) })
    ))
  })
  lazy val DestructuringAssignmentTarget: ESParser[DestructuringAssignmentTarget] = memo(args => {
    val List(pYield, pAwait) = getArgsN("DestructuringAssignmentTarget", args, 2)
    withSpan((
      withSpan(MATCH ~ LeftHandSideExpression(List(pYield, pAwait)) ^^ { case _ ~ x0 => DestructuringAssignmentTarget0(x0, args, Span()) })
    ))
  })
  lazy val Expression: ESParser[Expression] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("Expression", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ opt(comment) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => Expression0(x0, args, Span(rawPreComment = c)) })
    ), (
      log((MATCH ~ opt(comment) <~ t(",")) ~ AssignmentExpression(List(pIn, pYield, pAwait)) ^^ { case _ ~ c ~ x0 => ((x: Expression) => Expression1(x, x0, args, Span(rawPreComment = c))) })("Expression1")
    )))
  })
  lazy val Statement: ESParser[Statement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("Statement", args, 3)
    withSpan((
      withSpan(MATCH ~ BlockStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement0(x0, args, Span()) }) |
      withSpan(MATCH ~ VariableStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement1(x0, args, Span()) }) |
      withSpan(MATCH ~ EmptyStatement(List()) ^^ { case _ ~ x0 => Statement2(x0, args, Span()) }) |
      withSpan(MATCH ~ ExpressionStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement3(x0, args, Span()) }) |
      withSpan(MATCH ~ IfStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement4(x0, args, Span()) }) |
      withSpan(MATCH ~ BreakableStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement5(x0, args, Span()) }) |
      withSpan(MATCH ~ ContinueStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement6(x0, args, Span()) }) |
      withSpan(MATCH ~ BreakStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement7(x0, args, Span()) }) |
      withSpan((if (pReturn) MATCH ~ ReturnStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement8(x0, args, Span()) } else MISMATCH)) |
      withSpan(MATCH ~ WithStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement9(x0, args, Span()) }) |
      withSpan(MATCH ~ LabelledStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement10(x0, args, Span()) }) |
      withSpan(MATCH ~ ThrowStatement(List(pYield, pAwait)) ^^ { case _ ~ x0 => Statement11(x0, args, Span()) }) |
      withSpan(MATCH ~ TryStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Statement12(x0, args, Span()) }) |
      withSpan(MATCH ~ DebuggerStatement(List()) ^^ { case _ ~ x0 => Statement13(x0, args, Span()) })
    ))
  })
  lazy val Declaration: ESParser[Declaration] = memo(args => {
    val List(pYield, pAwait) = getArgsN("Declaration", args, 2)
    withSpan((
      withSpan(MATCH ~ HoistableDeclaration(List(pYield, pAwait, false)) ^^ { case _ ~ x0 => Declaration0(x0, args, Span()) }) |
      withSpan(MATCH ~ ClassDeclaration(List(pYield, pAwait, false)) ^^ { case _ ~ x0 => Declaration1(x0, args, Span()) }) |
      withSpan(MATCH ~ LexicalDeclaration(List(true, pYield, pAwait)) ^^ { case _ ~ x0 => Declaration2(x0, args, Span()) })
    ))
  })
  lazy val HoistableDeclaration: ESParser[HoistableDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("HoistableDeclaration", args, 3)
    withSpan((
      withSpan(MATCH ~ FunctionDeclaration(List(pYield, pAwait, pDefault)) ^^ { case _ ~ x0 => HoistableDeclaration0(x0, args, Span()) }) |
      withSpan(MATCH ~ GeneratorDeclaration(List(pYield, pAwait, pDefault)) ^^ { case _ ~ x0 => HoistableDeclaration1(x0, args, Span()) }) |
      withSpan(MATCH ~ AsyncFunctionDeclaration(List(pYield, pAwait, pDefault)) ^^ { case _ ~ x0 => HoistableDeclaration2(x0, args, Span()) }) |
      withSpan(MATCH ~ AsyncGeneratorDeclaration(List(pYield, pAwait, pDefault)) ^^ { case _ ~ x0 => HoistableDeclaration3(x0, args, Span()) })
    ))
  })
  lazy val BreakableStatement: ESParser[BreakableStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("BreakableStatement", args, 3)
    withSpan((
      withSpan(MATCH ~ IterationStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => BreakableStatement0(x0, args, Span()) }) |
      withSpan(MATCH ~ SwitchStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => BreakableStatement1(x0, args, Span()) })
    ))
  })
  lazy val BlockStatement: ESParser[BlockStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("BlockStatement", args, 3)
    withSpan((
      withSpan(MATCH ~ Block(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => BlockStatement0(x0, args, Span()) })
    ))
  })
  lazy val Block: ESParser[Block] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("Block", args, 3)
    withSpan((
      withSpan(((MATCH <~ t("{")) ~ opt(StatementList(List(pYield, pAwait, pReturn))) <~ t("}")) ^^ { case _ ~ x0 => Block0(x0, args, Span()) })
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
  lazy val StatementListItem: ESParser[StatementListItem] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("StatementListItem", args, 3)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ c ~ x0 => StatementListItem0(x0, args, Span(rawPreComment = c)) }) |
      withSpan(MATCH ~ opt(comment) ~ Declaration(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 => StatementListItem1(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val LexicalDeclaration: ESParser[LexicalDeclaration] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("LexicalDeclaration", args, 3)
    withSpan((
      withSpan((MATCH ~ LetOrConst(List()) ~ BindingList(List(pIn, pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 ~ x1 => LexicalDeclaration0(x0, x1, args, Span()) })
    ))
  })
  lazy val LetOrConst: ESParser[LetOrConst] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t("let")) ^^ { case _ => LetOrConst0(args, Span()) }) |
      withSpan((MATCH <~ t("const")) ^^ { case _ => LetOrConst1(args, Span()) })
    ))
  })
  lazy val BindingList: ESParser[BindingList] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("BindingList", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ LexicalBinding(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => BindingList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ LexicalBinding(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => ((x: BindingList) => BindingList1(x, x0, args, Span())) })("BindingList1")
    )))
  })
  lazy val LexicalBinding: ESParser[LexicalBinding] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("LexicalBinding", args, 3)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ~ opt(Initializer(List(pIn, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => LexicalBinding0(x0, x1, args, Span()) }) |
      withSpan(MATCH ~ BindingPattern(List(pYield, pAwait)) ~ Initializer(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => LexicalBinding1(x0, x1, args, Span()) })
    ))
  })
  lazy val VariableStatement: ESParser[VariableStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("VariableStatement", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("var")) ~ VariableDeclarationList(List(true, pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 => VariableStatement0(x0, args, Span()) })
    ))
  })
  lazy val VariableDeclarationList: ESParser[VariableDeclarationList] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("VariableDeclarationList", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ VariableDeclaration(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => VariableDeclarationList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ VariableDeclaration(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 => ((x: VariableDeclarationList) => VariableDeclarationList1(x, x0, args, Span())) })("VariableDeclarationList1")
    )))
  })
  lazy val VariableDeclaration: ESParser[VariableDeclaration] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("VariableDeclaration", args, 3)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ~ opt(Initializer(List(pIn, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => VariableDeclaration0(x0, x1, args, Span()) }) |
      withSpan(MATCH ~ BindingPattern(List(pYield, pAwait)) ~ Initializer(List(pIn, pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => VariableDeclaration1(x0, x1, args, Span()) })
    ))
  })
  lazy val BindingPattern: ESParser[BindingPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingPattern", args, 2)
    withSpan((
      withSpan(MATCH ~ ObjectBindingPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingPattern0(x0, args, Span()) }) |
      withSpan(MATCH ~ ArrayBindingPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingPattern1(x0, args, Span()) })
    ))
  })
  lazy val ObjectBindingPattern: ESParser[ObjectBindingPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ObjectBindingPattern", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("{")) <~ t("}")) ^^ { case _ => ObjectBindingPattern0(args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ BindingRestProperty(List(pYield, pAwait)) <~ t("}")) ^^ { case _ ~ x0 => ObjectBindingPattern1(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ BindingPropertyList(List(pYield, pAwait)) <~ t("}")) ^^ { case _ ~ x0 => ObjectBindingPattern2(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("{")) ~ BindingPropertyList(List(pYield, pAwait)) <~ t(",")) ~ opt(BindingRestProperty(List(pYield, pAwait))) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => ObjectBindingPattern3(x0, x1, args, Span()) })
    ))
  })
  lazy val ArrayBindingPattern: ESParser[ArrayBindingPattern] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArrayBindingPattern", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("[")) ~ opt(Elision(List())) ~ opt(BindingRestElement(List(pYield, pAwait))) <~ t("]")) ^^ { case _ ~ x0 ~ x1 => ArrayBindingPattern0(x0, x1, args, Span()) }) |
      withSpan(((MATCH <~ t("[")) ~ BindingElementList(List(pYield, pAwait)) <~ t("]")) ^^ { case _ ~ x0 => ArrayBindingPattern1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("[")) ~ BindingElementList(List(pYield, pAwait)) <~ t(",")) ~ opt(Elision(List())) ~ opt(BindingRestElement(List(pYield, pAwait))) <~ t("]")) ^^ { case _ ~ x0 ~ x1 ~ x2 => ArrayBindingPattern2(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val BindingRestProperty: ESParser[BindingRestProperty] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingRestProperty", args, 2)
    withSpan((
      withSpan((MATCH <~ t("...")) ~ BindingIdentifier(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingRestProperty0(x0, args, Span()) })
    ))
  })
  lazy val BindingPropertyList: ESParser[BindingPropertyList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingPropertyList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ BindingProperty(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingPropertyList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ BindingProperty(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: BindingPropertyList) => BindingPropertyList1(x, x0, args, Span())) })("BindingPropertyList1")
    )))
  })
  lazy val BindingElementList: ESParser[BindingElementList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingElementList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ BindingElisionElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingElementList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ BindingElisionElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: BindingElementList) => BindingElementList1(x, x0, args, Span())) })("BindingElementList1")
    )))
  })
  lazy val BindingElisionElement: ESParser[BindingElisionElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingElisionElement", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(Elision(List())) ~ BindingElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => BindingElisionElement0(x0, x1, args, Span()) })
    ))
  })
  lazy val BindingProperty: ESParser[BindingProperty] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingProperty", args, 2)
    withSpan((
      withSpan(MATCH ~ SingleNameBinding(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingProperty0(x0, args, Span()) }) |
      withSpan((MATCH ~ PropertyName(List(pYield, pAwait)) <~ t(":")) ~ BindingElement(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => BindingProperty1(x0, x1, args, Span()) })
    ))
  })
  lazy val BindingElement: ESParser[BindingElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingElement", args, 2)
    withSpan((
      withSpan(MATCH ~ SingleNameBinding(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingElement0(x0, args, Span()) }) |
      withSpan(MATCH ~ BindingPattern(List(pYield, pAwait)) ~ opt(Initializer(List(true, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => BindingElement1(x0, x1, args, Span()) })
    ))
  })
  lazy val SingleNameBinding: ESParser[SingleNameBinding] = memo(args => {
    val List(pYield, pAwait) = getArgsN("SingleNameBinding", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ~ opt(Initializer(List(true, pYield, pAwait))) ^^ { case _ ~ x0 ~ x1 => SingleNameBinding0(x0, x1, args, Span()) })
    ))
  })
  lazy val BindingRestElement: ESParser[BindingRestElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BindingRestElement", args, 2)
    withSpan((
      withSpan((MATCH <~ t("...")) ~ BindingIdentifier(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingRestElement0(x0, args, Span()) }) |
      withSpan((MATCH <~ t("...")) ~ BindingPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => BindingRestElement1(x0, args, Span()) })
    ))
  })
  lazy val EmptyStatement: ESParser[EmptyStatement] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t(";")) ^^ { case _ => EmptyStatement0(args, Span()) })
    ))
  })
  lazy val ExpressionStatement: ESParser[ExpressionStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ExpressionStatement", args, 2)
    withSpan((
      withSpan(((MATCH ~ opt(comment) <~ -ntl(("{") | (("function" <~ not(IDContinue))) | (("async" <~ not(IDContinue)) %% strNoLineTerminator %% ("function" <~ not(IDContinue))) | (("class" <~ not(IDContinue))) | (("let" <~ not(IDContinue)) %% "["))) ~ Expression(List(true, pYield, pAwait)) <~ t(";")) ^^ { case _ ~ c ~ x0 => ExpressionStatement0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val IfStatement: ESParser[IfStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("IfStatement", args, 3)
    withSpan((
      withSpan(((((MATCH <~ t("if")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) <~ t("else")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => IfStatement0(x0, x1, x2, args, Span()) }) |
      withSpan(((((MATCH <~ t("if")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) <~ -ntl((("else" <~ not(IDContinue))))) ^^ { case _ ~ x0 ~ x1 => IfStatement1(x0, x1, args, Span()) })
    ))
  })
  lazy val IterationStatement: ESParser[IterationStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("IterationStatement", args, 3)
    withSpan((
      withSpan(MATCH ~ DoWhileStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => IterationStatement0(x0, args, Span()) }) |
      withSpan(MATCH ~ WhileStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => IterationStatement1(x0, args, Span()) }) |
      withSpan(MATCH ~ ForStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => IterationStatement2(x0, args, Span()) }) |
      withSpan(MATCH ~ ForInOfStatement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => IterationStatement3(x0, args, Span()) })
    ))
  })
  lazy val DoWhileStatement: ESParser[DoWhileStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("DoWhileStatement", args, 3)
    withSpan((
      withSpan((((((MATCH <~ t("do")) ~ Statement(List(pYield, pAwait, pReturn)) <~ t("while")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) <~ t(";")) ^^ { case _ ~ x0 ~ x1 => DoWhileStatement0(x0, x1, args, Span()) })
    ))
  })
  lazy val WhileStatement: ESParser[WhileStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("WhileStatement", args, 3)
    withSpan((
      withSpan((((MATCH <~ t("while")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => WhileStatement0(x0, x1, args, Span()) })
    ))
  })
  lazy val ForStatement: ESParser[ForStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("ForStatement", args, 3)
    withSpan((
      withSpan(((((((MATCH <~ t("for")) <~ t("(")) <~ -ntl((("let" <~ not(IDContinue)) %% "["))) ~ opt(Expression(List(false, pYield, pAwait))) <~ t(";")) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(";")) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 ~ x3 => ForStatement0(x0, x1, x2, x3, args, Span()) }) |
      withSpan(((((((MATCH <~ t("for")) <~ t("(")) <~ t("var")) ~ VariableDeclarationList(List(false, pYield, pAwait)) <~ t(";")) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(";")) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 ~ x3 => ForStatement1(x0, x1, x2, x3, args, Span()) }) |
      withSpan(((((MATCH <~ t("for")) <~ t("(")) ~ LexicalDeclaration(List(false, pYield, pAwait)) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(";")) ~ opt(Expression(List(true, pYield, pAwait))) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 ~ x3 => ForStatement2(x0, x1, x2, x3, args, Span()) })
    ))
  })
  lazy val ForInOfStatement: ESParser[ForInOfStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("ForInOfStatement", args, 3)
    withSpan((
      withSpan((((((MATCH <~ t("for")) <~ t("(")) <~ -ntl((("let" <~ not(IDContinue)) %% "["))) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("in")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement0(x0, x1, x2, args, Span()) }) |
      withSpan((((((MATCH <~ t("for")) <~ t("(")) <~ t("var")) ~ ForBinding(List(pYield, pAwait)) <~ t("in")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement1(x0, x1, x2, args, Span()) }) |
      withSpan(((((MATCH <~ t("for")) <~ t("(")) ~ ForDeclaration(List(pYield, pAwait)) <~ t("in")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement2(x0, x1, x2, args, Span()) }) |
      withSpan((((((MATCH <~ t("for")) <~ t("(")) <~ -ntl((("let" <~ not(IDContinue))) | (("async" <~ not(IDContinue)) %% ("of" <~ not(IDContinue))))) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement3(x0, x1, x2, args, Span()) }) |
      withSpan((((((MATCH <~ t("for")) <~ t("(")) <~ t("var")) ~ ForBinding(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement4(x0, x1, x2, args, Span()) }) |
      withSpan(((((MATCH <~ t("for")) <~ t("(")) ~ ForDeclaration(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement5(x0, x1, x2, args, Span()) }) |
      withSpan((if (pAwait) ((((((MATCH <~ t("for")) <~ t("await")) <~ t("(")) <~ -ntl((("let" <~ not(IDContinue))))) ~ LeftHandSideExpression(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement6(x0, x1, x2, args, Span()) } else MISMATCH)) |
      withSpan((if (pAwait) ((((((MATCH <~ t("for")) <~ t("await")) <~ t("(")) <~ t("var")) ~ ForBinding(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement7(x0, x1, x2, args, Span()) } else MISMATCH)) |
      withSpan((if (pAwait) (((((MATCH <~ t("for")) <~ t("await")) <~ t("(")) ~ ForDeclaration(List(pYield, pAwait)) <~ t("of")) ~ AssignmentExpression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => ForInOfStatement8(x0, x1, x2, args, Span()) } else MISMATCH))
    ))
  })
  lazy val ForDeclaration: ESParser[ForDeclaration] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ForDeclaration", args, 2)
    withSpan((
      withSpan(MATCH ~ LetOrConst(List()) ~ ForBinding(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ForDeclaration0(x0, x1, args, Span()) })
    ))
  })
  lazy val ForBinding: ESParser[ForBinding] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ForBinding", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ^^ { case _ ~ x0 => ForBinding0(x0, args, Span()) }) |
      withSpan(MATCH ~ BindingPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => ForBinding1(x0, args, Span()) })
    ))
  })
  lazy val ContinueStatement: ESParser[ContinueStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ContinueStatement", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("continue")) <~ t(";")) ^^ { case _ => ContinueStatement0(args, Span()) }) |
      withSpan((((MATCH <~ t("continue")) <~ NoLineTerminator) ~ LabelIdentifier(List(pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 => ContinueStatement1(x0, args, Span()) })
    ))
  })
  lazy val BreakStatement: ESParser[BreakStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("BreakStatement", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("break")) <~ t(";")) ^^ { case _ => BreakStatement0(args, Span()) }) |
      withSpan((((MATCH <~ t("break")) <~ NoLineTerminator) ~ LabelIdentifier(List(pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 => BreakStatement1(x0, args, Span()) })
    ))
  })
  lazy val ReturnStatement: ESParser[ReturnStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ReturnStatement", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("return")) <~ t(";")) ^^ { case _ => ReturnStatement0(args, Span()) }) |
      withSpan((((MATCH <~ t("return")) <~ NoLineTerminator) ~ Expression(List(true, pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 => ReturnStatement1(x0, args, Span()) })
    ))
  })
  lazy val WithStatement: ESParser[WithStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("WithStatement", args, 3)
    withSpan((
      withSpan((((MATCH <~ t("with")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => WithStatement0(x0, x1, args, Span()) })
    ))
  })
  lazy val SwitchStatement: ESParser[SwitchStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("SwitchStatement", args, 3)
    withSpan((
      withSpan((((MATCH <~ t("switch")) <~ t("(")) ~ Expression(List(true, pYield, pAwait)) <~ t(")")) ~ CaseBlock(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => SwitchStatement0(x0, x1, args, Span()) })
    ))
  })
  lazy val CaseBlock: ESParser[CaseBlock] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("CaseBlock", args, 3)
    withSpan((
      withSpan(((MATCH <~ t("{")) ~ opt(CaseClauses(List(pYield, pAwait, pReturn))) <~ t("}")) ^^ { case _ ~ x0 => CaseBlock0(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ opt(CaseClauses(List(pYield, pAwait, pReturn))) ~ DefaultClause(List(pYield, pAwait, pReturn)) ~ opt(CaseClauses(List(pYield, pAwait, pReturn))) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => CaseBlock1(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val CaseClauses: ESParser[CaseClauses] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("CaseClauses", args, 3)
    withSpan(resolveLR((
      withSpan(MATCH ~ CaseClause(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => CaseClauses0(x0, args, Span()) })
    ), (
      log(MATCH ~ CaseClause(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => ((x: CaseClauses) => CaseClauses1(x, x0, args, Span())) })("CaseClauses1")
    )))
  })
  lazy val CaseClause: ESParser[CaseClause] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("CaseClause", args, 3)
    withSpan((
      withSpan(((MATCH <~ t("case")) ~ Expression(List(true, pYield, pAwait)) <~ t(":")) ~ opt(StatementList(List(pYield, pAwait, pReturn))) ^^ { case _ ~ x0 ~ x1 => CaseClause0(x0, x1, args, Span()) })
    ))
  })
  lazy val DefaultClause: ESParser[DefaultClause] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("DefaultClause", args, 3)
    withSpan((
      withSpan(((MATCH <~ t("default")) <~ t(":")) ~ opt(StatementList(List(pYield, pAwait, pReturn))) ^^ { case _ ~ x0 => DefaultClause0(x0, args, Span()) })
    ))
  })
  lazy val LabelledStatement: ESParser[LabelledStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("LabelledStatement", args, 3)
    withSpan((
      withSpan((MATCH ~ LabelIdentifier(List(pYield, pAwait)) <~ t(":")) ~ LabelledItem(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => LabelledStatement0(x0, x1, args, Span()) })
    ))
  })
  lazy val LabelledItem: ESParser[LabelledItem] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("LabelledItem", args, 3)
    withSpan((
      withSpan(MATCH ~ Statement(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => LabelledItem0(x0, args, Span()) }) |
      withSpan(MATCH ~ FunctionDeclaration(List(pYield, pAwait, false)) ^^ { case _ ~ x0 => LabelledItem1(x0, args, Span()) })
    ))
  })
  lazy val ThrowStatement: ESParser[ThrowStatement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ThrowStatement", args, 2)
    withSpan((
      withSpan((((MATCH <~ t("throw")) <~ NoLineTerminator) ~ Expression(List(true, pYield, pAwait)) <~ t(";")) ^^ { case _ ~ x0 => ThrowStatement0(x0, args, Span()) })
    ))
  })
  lazy val TryStatement: ESParser[TryStatement] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("TryStatement", args, 3)
    withSpan((
      withSpan((MATCH <~ t("try")) ~ Block(List(pYield, pAwait, pReturn)) ~ Catch(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => TryStatement0(x0, x1, args, Span()) }) |
      withSpan((MATCH <~ t("try")) ~ Block(List(pYield, pAwait, pReturn)) ~ Finally(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => TryStatement1(x0, x1, args, Span()) }) |
      withSpan((MATCH <~ t("try")) ~ Block(List(pYield, pAwait, pReturn)) ~ Catch(List(pYield, pAwait, pReturn)) ~ Finally(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 ~ x2 => TryStatement2(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val Catch: ESParser[Catch] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("Catch", args, 3)
    withSpan((
      withSpan((((MATCH <~ t("catch")) <~ t("(")) ~ CatchParameter(List(pYield, pAwait)) <~ t(")")) ~ Block(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 ~ x1 => Catch0(x0, x1, args, Span()) }) |
      withSpan((MATCH <~ t("catch")) ~ Block(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Catch1(x0, args, Span()) })
    ))
  })
  lazy val Finally: ESParser[Finally] = memo(args => {
    val List(pYield, pAwait, pReturn) = getArgsN("Finally", args, 3)
    withSpan((
      withSpan((MATCH <~ t("finally")) ~ Block(List(pYield, pAwait, pReturn)) ^^ { case _ ~ x0 => Finally0(x0, args, Span()) })
    ))
  })
  lazy val CatchParameter: ESParser[CatchParameter] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CatchParameter", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ^^ { case _ ~ x0 => CatchParameter0(x0, args, Span()) }) |
      withSpan(MATCH ~ BindingPattern(List(pYield, pAwait)) ^^ { case _ ~ x0 => CatchParameter1(x0, args, Span()) })
    ))
  })
  lazy val DebuggerStatement: ESParser[DebuggerStatement] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("debugger")) <~ t(";")) ^^ { case _ => DebuggerStatement0(args, Span()) })
    ))
  })
  lazy val UniqueFormalParameters: ESParser[UniqueFormalParameters] = memo(args => {
    val List(pYield, pAwait) = getArgsN("UniqueFormalParameters", args, 2)
    withSpan((
      withSpan(MATCH ~ FormalParameters(List(pYield, pAwait)) ^^ { case _ ~ x0 => UniqueFormalParameters0(x0, args, Span()) })
    ))
  })
  lazy val FormalParameters: ESParser[FormalParameters] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FormalParameters", args, 2)
    withSpan((
      withSpan(MATCH ~ MATCH ^^ { case _ => FormalParameters0(args, Span()) }) |
      withSpan(MATCH ~ FunctionRestParameter(List(pYield, pAwait)) ^^ { case _ ~ x0 => FormalParameters1(x0, args, Span()) }) |
      withSpan(MATCH ~ FormalParameterList(List(pYield, pAwait)) ^^ { case _ ~ x0 => FormalParameters2(x0, args, Span()) }) |
      withSpan((MATCH ~ FormalParameterList(List(pYield, pAwait)) <~ t(",")) ^^ { case _ ~ x0 => FormalParameters3(x0, args, Span()) }) |
      withSpan((MATCH ~ FormalParameterList(List(pYield, pAwait)) <~ t(",")) ~ FunctionRestParameter(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => FormalParameters4(x0, x1, args, Span()) })
    ))
  })
  lazy val FormalParameterList: ESParser[FormalParameterList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FormalParameterList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ FormalParameter(List(pYield, pAwait)) ^^ { case _ ~ x0 => FormalParameterList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ FormalParameter(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: FormalParameterList) => FormalParameterList1(x, x0, args, Span())) })("FormalParameterList1")
    )))
  })
  lazy val FunctionRestParameter: ESParser[FunctionRestParameter] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FunctionRestParameter", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingRestElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => FunctionRestParameter0(x0, args, Span()) })
    ))
  })
  lazy val FormalParameter: ESParser[FormalParameter] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FormalParameter", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => FormalParameter0(x0, args, Span()) })
    ))
  })
  lazy val FunctionDeclaration: ESParser[FunctionDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("FunctionDeclaration", args, 3)
    withSpan((
      withSpan((((((MATCH <~ t("function")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t("(")) ~ FormalParameters(List(false, false)) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => FunctionDeclaration0(x0, x1, x2, args, Span()) }) |
      withSpan((if (pDefault) (((((MATCH <~ t("function")) <~ t("(")) ~ FormalParameters(List(false, false)) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => FunctionDeclaration1(x0, x1, args, Span()) } else MISMATCH))
    ))
  })
  lazy val FunctionExpression: ESParser[FunctionExpression] = memo(args => {
    withSpan((
      withSpan((((((MATCH ~ opt(comment) <~ t("function")) ~ opt(BindingIdentifier(List(false, false))) <~ t("(")) ~ FormalParameters(List(false, false)) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => FunctionExpression0(x0, x1, x2, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val FunctionBody: ESParser[FunctionBody] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FunctionBody", args, 2)
    withSpan((
      withSpan(MATCH ~ FunctionStatementList(List(pYield, pAwait)) ^^ { case _ ~ x0 => FunctionBody0(x0, args, Span()) })
    ))
  })
  lazy val FunctionStatementList: ESParser[FunctionStatementList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("FunctionStatementList", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(StatementList(List(pYield, pAwait, true))) ^^ { case _ ~ x0 => FunctionStatementList0(x0, args, Span()) })
    ))
  })
  lazy val ArrowFunction: ESParser[ArrowFunction] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("ArrowFunction", args, 3)
    withSpan((
      withSpan(((MATCH ~ ArrowParameters(List(pYield, pAwait)) <~ NoLineTerminator) <~ t("=>")) ~ ConciseBody(List(pIn)) ^^ { case _ ~ x0 ~ x1 => ArrowFunction0(x0, x1, args, Span()) })
    ))
  })
  lazy val ArrowParameters: ESParser[ArrowParameters] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArrowParameters", args, 2)
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(pYield, pAwait)) ^^ { case _ ~ x0 => ArrowParameters0(x0, args, Span()) }) |
      withSpan(MATCH ~ CoverParenthesizedExpressionAndArrowParameterList(List(pYield, pAwait)) ^^ { case _ ~ x0 => ArrowParameters1(x0, args, Span()) })
    ))
  })
  lazy val ConciseBody: ESParser[ConciseBody] = memo(args => {
    val List(pIn) = getArgsN("ConciseBody", args, 1)
    withSpan((
      withSpan((MATCH <~ -ntl(("{"))) ~ ExpressionBody(List(pIn, false)) ^^ { case _ ~ x0 => ConciseBody0(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 => ConciseBody1(x0, args, Span()) })
    ))
  })
  lazy val ExpressionBody: ESParser[ExpressionBody] = memo(args => {
    val List(pIn, pAwait) = getArgsN("ExpressionBody", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ AssignmentExpression(List(pIn, false, pAwait)) ^^ { case _ ~ c ~ x0 => ExpressionBody0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val ArrowFormalParameters: ESParser[ArrowFormalParameters] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ArrowFormalParameters", args, 2)
    withSpan((
      withSpan(((MATCH <~ t("(")) ~ UniqueFormalParameters(List(pYield, pAwait)) <~ t(")")) ^^ { case _ ~ x0 => ArrowFormalParameters0(x0, args, Span()) })
    ))
  })
  lazy val MethodDefinition: ESParser[MethodDefinition] = memo(args => {
    val List(pYield, pAwait) = getArgsN("MethodDefinition", args, 2)
    withSpan((
      withSpan(((((MATCH ~ PropertyName(List(pYield, pAwait)) <~ t("(")) ~ UniqueFormalParameters(List(false, false)) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => MethodDefinition0(x0, x1, x2, args, Span()) }) |
      withSpan(MATCH ~ GeneratorMethod(List(pYield, pAwait)) ^^ { case _ ~ x0 => MethodDefinition1(x0, args, Span()) }) |
      withSpan(MATCH ~ AsyncMethod(List(pYield, pAwait)) ^^ { case _ ~ x0 => MethodDefinition2(x0, args, Span()) }) |
      withSpan(MATCH ~ AsyncGeneratorMethod(List(pYield, pAwait)) ^^ { case _ ~ x0 => MethodDefinition3(x0, args, Span()) }) |
      withSpan((((((MATCH <~ t("get")) ~ PropertyName(List(pYield, pAwait)) <~ t("(")) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => MethodDefinition4(x0, x1, args, Span()) }) |
      withSpan((((((MATCH <~ t("set")) ~ PropertyName(List(pYield, pAwait)) <~ t("(")) ~ PropertySetParameterList(List()) <~ t(")")) <~ t("{")) ~ FunctionBody(List(false, false)) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => MethodDefinition5(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val PropertySetParameterList: ESParser[PropertySetParameterList] = memo(args => {
    withSpan((
      withSpan(MATCH ~ FormalParameter(List(false, false)) ^^ { case _ ~ x0 => PropertySetParameterList0(x0, args, Span()) })
    ))
  })
  lazy val GeneratorMethod: ESParser[GeneratorMethod] = memo(args => {
    val List(pYield, pAwait) = getArgsN("GeneratorMethod", args, 2)
    withSpan((
      withSpan((((((MATCH <~ t("*")) ~ PropertyName(List(pYield, pAwait)) <~ t("(")) ~ UniqueFormalParameters(List(true, false)) <~ t(")")) <~ t("{")) ~ GeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => GeneratorMethod0(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val GeneratorDeclaration: ESParser[GeneratorDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("GeneratorDeclaration", args, 3)
    withSpan((
      withSpan(((((((MATCH <~ t("function")) <~ t("*")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t("(")) ~ FormalParameters(List(true, false)) <~ t(")")) <~ t("{")) ~ GeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => GeneratorDeclaration0(x0, x1, x2, args, Span()) }) |
      withSpan((if (pDefault) ((((((MATCH <~ t("function")) <~ t("*")) <~ t("(")) ~ FormalParameters(List(true, false)) <~ t(")")) <~ t("{")) ~ GeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => GeneratorDeclaration1(x0, x1, args, Span()) } else MISMATCH))
    ))
  })
  lazy val GeneratorExpression: ESParser[GeneratorExpression] = memo(args => {
    withSpan((
      withSpan(((((((MATCH ~ opt(comment) <~ t("function")) <~ t("*")) ~ opt(BindingIdentifier(List(true, false))) <~ t("(")) ~ FormalParameters(List(true, false)) <~ t(")")) <~ t("{")) ~ GeneratorBody(List()) <~ t("}")) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => GeneratorExpression0(x0, x1, x2, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val GeneratorBody: ESParser[GeneratorBody] = memo(args => {
    withSpan((
      withSpan(MATCH ~ FunctionBody(List(true, false)) ^^ { case _ ~ x0 => GeneratorBody0(x0, args, Span()) })
    ))
  })
  lazy val YieldExpression: ESParser[YieldExpression] = memo(args => {
    val List(pIn, pAwait) = getArgsN("YieldExpression", args, 2)
    withSpan((
      withSpan((MATCH ~ opt(comment) <~ t("yield")) ^^ { case _ ~ c => YieldExpression0(args, Span(rawPreComment = c)) }) |
      withSpan(((MATCH ~ opt(comment) <~ t("yield")) <~ NoLineTerminator) ~ AssignmentExpression(List(pIn, true, pAwait)) ^^ { case _ ~ c ~ x0 => YieldExpression1(x0, args, Span(rawPreComment = c)) }) |
      withSpan((((MATCH ~ opt(comment) <~ t("yield")) <~ NoLineTerminator) <~ t("*")) ~ AssignmentExpression(List(pIn, true, pAwait)) ^^ { case _ ~ c ~ x0 => YieldExpression2(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AsyncGeneratorMethod: ESParser[AsyncGeneratorMethod] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AsyncGeneratorMethod", args, 2)
    withSpan((
      withSpan((((((((MATCH <~ t("async")) <~ NoLineTerminator) <~ t("*")) ~ PropertyName(List(pYield, pAwait)) <~ t("(")) ~ UniqueFormalParameters(List(true, true)) <~ t(")")) <~ t("{")) ~ AsyncGeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => AsyncGeneratorMethod0(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val AsyncGeneratorDeclaration: ESParser[AsyncGeneratorDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("AsyncGeneratorDeclaration", args, 3)
    withSpan((
      withSpan(((((((((MATCH <~ t("async")) <~ NoLineTerminator) <~ t("function")) <~ t("*")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t("(")) ~ FormalParameters(List(true, true)) <~ t(")")) <~ t("{")) ~ AsyncGeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => AsyncGeneratorDeclaration0(x0, x1, x2, args, Span()) }) |
      withSpan((if (pDefault) ((((((((MATCH <~ t("async")) <~ NoLineTerminator) <~ t("function")) <~ t("*")) <~ t("(")) ~ FormalParameters(List(true, true)) <~ t(")")) <~ t("{")) ~ AsyncGeneratorBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => AsyncGeneratorDeclaration1(x0, x1, args, Span()) } else MISMATCH))
    ))
  })
  lazy val AsyncGeneratorExpression: ESParser[AsyncGeneratorExpression] = memo(args => {
    withSpan((
      withSpan(((((((((MATCH ~ opt(comment) <~ t("async")) <~ NoLineTerminator) <~ t("function")) <~ t("*")) ~ opt(BindingIdentifier(List(true, true))) <~ t("(")) ~ FormalParameters(List(true, true)) <~ t(")")) <~ t("{")) ~ AsyncGeneratorBody(List()) <~ t("}")) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => AsyncGeneratorExpression0(x0, x1, x2, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AsyncGeneratorBody: ESParser[AsyncGeneratorBody] = memo(args => {
    withSpan((
      withSpan(MATCH ~ FunctionBody(List(true, true)) ^^ { case _ ~ x0 => AsyncGeneratorBody0(x0, args, Span()) })
    ))
  })
  lazy val ClassDeclaration: ESParser[ClassDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("ClassDeclaration", args, 3)
    withSpan((
      withSpan((MATCH <~ t("class")) ~ BindingIdentifier(List(pYield, pAwait)) ~ ClassTail(List(pYield, pAwait)) ^^ { case _ ~ x0 ~ x1 => ClassDeclaration0(x0, x1, args, Span()) }) |
      withSpan((if (pDefault) (MATCH <~ t("class")) ~ ClassTail(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassDeclaration1(x0, args, Span()) } else MISMATCH))
    ))
  })
  lazy val ClassExpression: ESParser[ClassExpression] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassExpression", args, 2)
    withSpan((
      withSpan((MATCH ~ opt(comment) <~ t("class")) ~ opt(BindingIdentifier(List(pYield, pAwait))) ~ ClassTail(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => ClassExpression0(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val ClassTail: ESParser[ClassTail] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassTail", args, 2)
    withSpan((
      withSpan(((MATCH ~ opt(ClassHeritage(List(pYield, pAwait))) <~ t("{")) ~ opt(ClassBody(List(pYield, pAwait))) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => ClassTail0(x0, x1, args, Span()) })
    ))
  })
  lazy val ClassHeritage: ESParser[ClassHeritage] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassHeritage", args, 2)
    withSpan((
      withSpan((MATCH <~ t("extends")) ~ LeftHandSideExpression(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassHeritage0(x0, args, Span()) })
    ))
  })
  lazy val ClassBody: ESParser[ClassBody] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassBody", args, 2)
    withSpan((
      withSpan(MATCH ~ ClassElementList(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassBody0(x0, args, Span()) })
    ))
  })
  lazy val ClassElementList: ESParser[ClassElementList] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassElementList", args, 2)
    withSpan(resolveLR((
      withSpan(MATCH ~ ClassElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassElementList0(x0, args, Span()) })
    ), (
      log(MATCH ~ ClassElement(List(pYield, pAwait)) ^^ { case _ ~ x0 => ((x: ClassElementList) => ClassElementList1(x, x0, args, Span())) })("ClassElementList1")
    )))
  })
  lazy val ClassElement: ESParser[ClassElement] = memo(args => {
    val List(pYield, pAwait) = getArgsN("ClassElement", args, 2)
    withSpan((
      withSpan(MATCH ~ MethodDefinition(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassElement0(x0, args, Span()) }) |
      withSpan((MATCH <~ t("static")) ~ MethodDefinition(List(pYield, pAwait)) ^^ { case _ ~ x0 => ClassElement1(x0, args, Span()) }) |
      withSpan((MATCH <~ t(";")) ^^ { case _ => ClassElement2(args, Span()) })
    ))
  })
  lazy val AsyncFunctionDeclaration: ESParser[AsyncFunctionDeclaration] = memo(args => {
    val List(pYield, pAwait, pDefault) = getArgsN("AsyncFunctionDeclaration", args, 3)
    withSpan((
      withSpan((((((((MATCH <~ t("async")) <~ NoLineTerminator) <~ t("function")) ~ BindingIdentifier(List(pYield, pAwait)) <~ t("(")) ~ FormalParameters(List(false, true)) <~ t(")")) <~ t("{")) ~ AsyncFunctionBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => AsyncFunctionDeclaration0(x0, x1, x2, args, Span()) }) |
      withSpan((if (pDefault) (((((((MATCH <~ t("async")) <~ NoLineTerminator) <~ t("function")) <~ t("(")) ~ FormalParameters(List(false, true)) <~ t(")")) <~ t("{")) ~ AsyncFunctionBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 => AsyncFunctionDeclaration1(x0, x1, args, Span()) } else MISMATCH))
    ))
  })
  lazy val AsyncFunctionExpression: ESParser[AsyncFunctionExpression] = memo(args => {
    withSpan((
      withSpan((((((((MATCH ~ opt(comment) <~ t("async")) <~ NoLineTerminator) <~ t("function")) ~ opt(BindingIdentifier(List(false, true))) <~ t("(")) ~ FormalParameters(List(false, true)) <~ t(")")) <~ t("{")) ~ AsyncFunctionBody(List()) <~ t("}")) ^^ { case _ ~ c ~ x0 ~ x1 ~ x2 => AsyncFunctionExpression0(x0, x1, x2, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AsyncMethod: ESParser[AsyncMethod] = memo(args => {
    val List(pYield, pAwait) = getArgsN("AsyncMethod", args, 2)
    withSpan((
      withSpan(((((((MATCH <~ t("async")) <~ NoLineTerminator) ~ PropertyName(List(pYield, pAwait)) <~ t("(")) ~ UniqueFormalParameters(List(false, true)) <~ t(")")) <~ t("{")) ~ AsyncFunctionBody(List()) <~ t("}")) ^^ { case _ ~ x0 ~ x1 ~ x2 => AsyncMethod0(x0, x1, x2, args, Span()) })
    ))
  })
  lazy val AsyncFunctionBody: ESParser[AsyncFunctionBody] = memo(args => {
    withSpan((
      withSpan(MATCH ~ FunctionBody(List(false, true)) ^^ { case _ ~ x0 => AsyncFunctionBody0(x0, args, Span()) })
    ))
  })
  lazy val AwaitExpression: ESParser[AwaitExpression] = memo(args => {
    val List(pYield) = getArgsN("AwaitExpression", args, 1)
    withSpan((
      withSpan((MATCH ~ opt(comment) <~ t("await")) ~ UnaryExpression(List(pYield, true)) ^^ { case _ ~ c ~ x0 => AwaitExpression0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AsyncArrowFunction: ESParser[AsyncArrowFunction] = memo(args => {
    val List(pIn, pYield, pAwait) = getArgsN("AsyncArrowFunction", args, 3)
    withSpan((
      withSpan(((((MATCH <~ t("async")) <~ NoLineTerminator) ~ AsyncArrowBindingIdentifier(List(pYield)) <~ NoLineTerminator) <~ t("=>")) ~ AsyncConciseBody(List(pIn)) ^^ { case _ ~ x0 ~ x1 => AsyncArrowFunction0(x0, x1, args, Span()) }) |
      withSpan(((MATCH ~ CoverCallExpressionAndAsyncArrowHead(List(pYield, pAwait)) <~ NoLineTerminator) <~ t("=>")) ~ AsyncConciseBody(List(pIn)) ^^ { case _ ~ x0 ~ x1 => AsyncArrowFunction1(x0, x1, args, Span()) })
    ))
  })
  lazy val AsyncConciseBody: ESParser[AsyncConciseBody] = memo(args => {
    val List(pIn) = getArgsN("AsyncConciseBody", args, 1)
    withSpan((
      withSpan((MATCH <~ -ntl(("{"))) ~ ExpressionBody(List(pIn, true)) ^^ { case _ ~ x0 => AsyncConciseBody0(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ AsyncFunctionBody(List()) <~ t("}")) ^^ { case _ ~ x0 => AsyncConciseBody1(x0, args, Span()) })
    ))
  })
  lazy val AsyncArrowBindingIdentifier: ESParser[AsyncArrowBindingIdentifier] = memo(args => {
    val List(pYield) = getArgsN("AsyncArrowBindingIdentifier", args, 1)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ BindingIdentifier(List(pYield, true)) ^^ { case _ ~ c ~ x0 => AsyncArrowBindingIdentifier0(x0, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val CoverCallExpressionAndAsyncArrowHead: ESParser[CoverCallExpressionAndAsyncArrowHead] = memo(args => {
    val List(pYield, pAwait) = getArgsN("CoverCallExpressionAndAsyncArrowHead", args, 2)
    withSpan((
      withSpan(MATCH ~ opt(comment) ~ MemberExpression(List(pYield, pAwait)) ~ Arguments(List(pYield, pAwait)) ^^ { case _ ~ c ~ x0 ~ x1 => CoverCallExpressionAndAsyncArrowHead0(x0, x1, args, Span(rawPreComment = c)) })
    ))
  })
  lazy val AsyncArrowHead: ESParser[AsyncArrowHead] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("async")) <~ NoLineTerminator) ~ ArrowFormalParameters(List(false, true)) ^^ { case _ ~ x0 => AsyncArrowHead0(x0, args, Span()) })
    ))
  })
  lazy val Script: ESParser[Script] = memo(args => {
    withSpan((
      withSpan(MATCH ~ opt(ScriptBody(List())) ^^ { case _ ~ x0 => Script0(x0, args, Span()) })
    ))
  })
  lazy val ScriptBody: ESParser[ScriptBody] = memo(args => {
    withSpan((
      withSpan(MATCH ~ StatementList(List(false, false, false)) ^^ { case _ ~ x0 => ScriptBody0(x0, args, Span()) })
    ))
  })
  lazy val Module: ESParser[Module] = memo(args => {
    withSpan((
      withSpan(MATCH ~ opt(ModuleBody(List())) ^^ { case _ ~ x0 => Module0(x0, args, Span()) })
    ))
  })
  lazy val ModuleBody: ESParser[ModuleBody] = memo(args => {
    withSpan((
      withSpan(MATCH ~ ModuleItemList(List()) ^^ { case _ ~ x0 => ModuleBody0(x0, args, Span()) })
    ))
  })
  lazy val ModuleItemList: ESParser[ModuleItemList] = memo(args => {
    withSpan(resolveLR((
      withSpan(MATCH ~ ModuleItem(List()) ^^ { case _ ~ x0 => ModuleItemList0(x0, args, Span()) })
    ), (
      log(MATCH ~ ModuleItem(List()) ^^ { case _ ~ x0 => ((x: ModuleItemList) => ModuleItemList1(x, x0, args, Span())) })("ModuleItemList1")
    )))
  })
  lazy val ModuleItem: ESParser[ModuleItem] = memo(args => {
    withSpan((
      withSpan(MATCH ~ ImportDeclaration(List()) ^^ { case _ ~ x0 => ModuleItem0(x0, args, Span()) }) |
      withSpan(MATCH ~ ExportDeclaration(List()) ^^ { case _ ~ x0 => ModuleItem1(x0, args, Span()) }) |
      withSpan(MATCH ~ StatementListItem(List(false, false, false)) ^^ { case _ ~ x0 => ModuleItem2(x0, args, Span()) })
    ))
  })
  lazy val ImportDeclaration: ESParser[ImportDeclaration] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("import")) ~ ImportClause(List()) ~ FromClause(List()) <~ t(";")) ^^ { case _ ~ x0 ~ x1 => ImportDeclaration0(x0, x1, args, Span()) }) |
      withSpan(((MATCH <~ t("import")) ~ ModuleSpecifier(List()) <~ t(";")) ^^ { case _ ~ x0 => ImportDeclaration1(x0, args, Span()) })
    ))
  })
  lazy val ImportClause: ESParser[ImportClause] = memo(args => {
    withSpan((
      withSpan(MATCH ~ ImportedDefaultBinding(List()) ^^ { case _ ~ x0 => ImportClause0(x0, args, Span()) }) |
      withSpan(MATCH ~ NameSpaceImport(List()) ^^ { case _ ~ x0 => ImportClause1(x0, args, Span()) }) |
      withSpan(MATCH ~ NamedImports(List()) ^^ { case _ ~ x0 => ImportClause2(x0, args, Span()) }) |
      withSpan((MATCH ~ ImportedDefaultBinding(List()) <~ t(",")) ~ NameSpaceImport(List()) ^^ { case _ ~ x0 ~ x1 => ImportClause3(x0, x1, args, Span()) }) |
      withSpan((MATCH ~ ImportedDefaultBinding(List()) <~ t(",")) ~ NamedImports(List()) ^^ { case _ ~ x0 ~ x1 => ImportClause4(x0, x1, args, Span()) })
    ))
  })
  lazy val ImportedDefaultBinding: ESParser[ImportedDefaultBinding] = memo(args => {
    withSpan((
      withSpan(MATCH ~ ImportedBinding(List()) ^^ { case _ ~ x0 => ImportedDefaultBinding0(x0, args, Span()) })
    ))
  })
  lazy val NameSpaceImport: ESParser[NameSpaceImport] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("*")) <~ t("as")) ~ ImportedBinding(List()) ^^ { case _ ~ x0 => NameSpaceImport0(x0, args, Span()) })
    ))
  })
  lazy val NamedImports: ESParser[NamedImports] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("{")) <~ t("}")) ^^ { case _ => NamedImports0(args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ ImportsList(List()) <~ t("}")) ^^ { case _ ~ x0 => NamedImports1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("{")) ~ ImportsList(List()) <~ t(",")) <~ t("}")) ^^ { case _ ~ x0 => NamedImports2(x0, args, Span()) })
    ))
  })
  lazy val FromClause: ESParser[FromClause] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t("from")) ~ ModuleSpecifier(List()) ^^ { case _ ~ x0 => FromClause0(x0, args, Span()) })
    ))
  })
  lazy val ImportsList: ESParser[ImportsList] = memo(args => {
    withSpan(resolveLR((
      withSpan(MATCH ~ ImportSpecifier(List()) ^^ { case _ ~ x0 => ImportsList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ ImportSpecifier(List()) ^^ { case _ ~ x0 => ((x: ImportsList) => ImportsList1(x, x0, args, Span())) })("ImportsList1")
    )))
  })
  lazy val ImportSpecifier: ESParser[ImportSpecifier] = memo(args => {
    withSpan((
      withSpan(MATCH ~ ImportedBinding(List()) ^^ { case _ ~ x0 => ImportSpecifier0(x0, args, Span()) }) |
      withSpan((MATCH ~ nt("IdentifierName", IdentifierName) <~ t("as")) ~ ImportedBinding(List()) ^^ { case _ ~ x0 ~ x1 => ImportSpecifier1(x0, x1, args, Span()) })
    ))
  })
  lazy val ModuleSpecifier: ESParser[ModuleSpecifier] = memo(args => {
    withSpan((
      withSpan(MATCH ~ nt("StringLiteral", StringLiteral) ^^ { case _ ~ x0 => ModuleSpecifier0(x0, args, Span()) })
    ))
  })
  lazy val ImportedBinding: ESParser[ImportedBinding] = memo(args => {
    withSpan((
      withSpan(MATCH ~ BindingIdentifier(List(false, false)) ^^ { case _ ~ x0 => ImportedBinding0(x0, args, Span()) })
    ))
  })
  lazy val ExportDeclaration: ESParser[ExportDeclaration] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("export")) ~ ExportFromClause(List()) ~ FromClause(List()) <~ t(";")) ^^ { case _ ~ x0 ~ x1 => ExportDeclaration0(x0, x1, args, Span()) }) |
      withSpan(((MATCH <~ t("export")) ~ NamedExports(List()) <~ t(";")) ^^ { case _ ~ x0 => ExportDeclaration1(x0, args, Span()) }) |
      withSpan((MATCH <~ t("export")) ~ VariableStatement(List(false, false)) ^^ { case _ ~ x0 => ExportDeclaration2(x0, args, Span()) }) |
      withSpan((MATCH <~ t("export")) ~ Declaration(List(false, false)) ^^ { case _ ~ x0 => ExportDeclaration3(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("export")) <~ t("default")) ~ HoistableDeclaration(List(false, false, true)) ^^ { case _ ~ x0 => ExportDeclaration4(x0, args, Span()) }) |
      withSpan(((MATCH <~ t("export")) <~ t("default")) ~ ClassDeclaration(List(false, false, true)) ^^ { case _ ~ x0 => ExportDeclaration5(x0, args, Span()) }) |
      withSpan(((((MATCH <~ t("export")) <~ t("default")) <~ -ntl((("function" <~ not(IDContinue))) | (("async" <~ not(IDContinue)) %% strNoLineTerminator %% ("function" <~ not(IDContinue))) | (("class" <~ not(IDContinue))))) ~ AssignmentExpression(List(true, false, false)) <~ t(";")) ^^ { case _ ~ x0 => ExportDeclaration6(x0, args, Span()) })
    ))
  })
  lazy val ExportFromClause: ESParser[ExportFromClause] = memo(args => {
    withSpan((
      withSpan((MATCH <~ t("*")) ^^ { case _ => ExportFromClause0(args, Span()) }) |
      withSpan(((MATCH <~ t("*")) <~ t("as")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => ExportFromClause1(x0, args, Span()) }) |
      withSpan(MATCH ~ NamedExports(List()) ^^ { case _ ~ x0 => ExportFromClause2(x0, args, Span()) })
    ))
  })
  lazy val NamedExports: ESParser[NamedExports] = memo(args => {
    withSpan((
      withSpan(((MATCH <~ t("{")) <~ t("}")) ^^ { case _ => NamedExports0(args, Span()) }) |
      withSpan(((MATCH <~ t("{")) ~ ExportsList(List()) <~ t("}")) ^^ { case _ ~ x0 => NamedExports1(x0, args, Span()) }) |
      withSpan((((MATCH <~ t("{")) ~ ExportsList(List()) <~ t(",")) <~ t("}")) ^^ { case _ ~ x0 => NamedExports2(x0, args, Span()) })
    ))
  })
  lazy val ExportsList: ESParser[ExportsList] = memo(args => {
    withSpan(resolveLR((
      withSpan(MATCH ~ ExportSpecifier(List()) ^^ { case _ ~ x0 => ExportsList0(x0, args, Span()) })
    ), (
      log((MATCH <~ t(",")) ~ ExportSpecifier(List()) ^^ { case _ ~ x0 => ((x: ExportsList) => ExportsList1(x, x0, args, Span())) })("ExportsList1")
    )))
  })
  lazy val ExportSpecifier: ESParser[ExportSpecifier] = memo(args => {
    withSpan((
      withSpan(MATCH ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 => ExportSpecifier0(x0, args, Span()) }) |
      withSpan((MATCH ~ nt("IdentifierName", IdentifierName) <~ t("as")) ~ nt("IdentifierName", IdentifierName) ^^ { case _ ~ x0 ~ x1 => ExportSpecifier1(x0, x1, args, Span()) })
    ))
  })
  val TERMINAL: Lexer = (
    "%=" |||
    "^=" |||
    "?" |||
    "var" |||
    "import" |||
    ">>>=" |||
    "void" |||
    "default" |||
    "static" |||
    "continue" |||
    "meta" |||
    "+" |||
    "get" |||
    "extends" |||
    "{" |||
    ";" |||
    "/" |||
    "/=" |||
    "&&" |||
    "of" |||
    "debugger" |||
    "let" |||
    "try" |||
    "..." |||
    "with" |||
    "-=" |||
    "]" |||
    ">>" |||
    "!" |||
    "<<=" |||
    "|=" |||
    ">>=" |||
    "switch" |||
    "<" |||
    "finally" |||
    "super" |||
    "=>" |||
    "," |||
    "delete" |||
    ":" |||
    "while" |||
    "&" |||
    "*" |||
    "function" |||
    ">" |||
    "export" |||
    "+=" |||
    "." |||
    "^" |||
    ">>>" |||
    "?." <~ not(DecimalDigit) |||
    "%" |||
    "const" |||
    "await" |||
    "===" |||
    ")" |||
    "-" |||
    "--" |||
    "new" |||
    "++" |||
    "??" |||
    "!=" |||
    "[" |||
    "target" |||
    "throw" |||
    "if" |||
    ">=" |||
    "??=" |||
    "&=" |||
    "catch" |||
    "async" |||
    "||" |||
    "break" |||
    "==" |||
    "=" |||
    "set" |||
    "|" |||
    "**" |||
    "(" |||
    "}" |||
    "**=" |||
    "this" |||
    "as" |||
    "typeof" |||
    "&&=" |||
    "*=" |||
    "<=" |||
    "else" |||
    "case" |||
    "~" |||
    "<<" |||
    "yield" |||
    "for" |||
    "return" |||
    "!==" |||
    "from" |||
    "in" |||
    "instanceof" |||
    "do" |||
    "||=" |||
    "class"
  )
  val rules: Map[String, ESParser[AST]] = Map(
    "IdentifierReference" -> IdentifierReference,
    "BindingIdentifier" -> BindingIdentifier,
    "LabelIdentifier" -> LabelIdentifier,
    "Identifier" -> Identifier,
    "PrimaryExpression" -> PrimaryExpression,
    "CoverParenthesizedExpressionAndArrowParameterList" -> CoverParenthesizedExpressionAndArrowParameterList,
    "ParenthesizedExpression" -> ParenthesizedExpression,
    "Literal" -> Literal,
    "ArrayLiteral" -> ArrayLiteral,
    "ElementList" -> ElementList,
    "Elision" -> Elision,
    "SpreadElement" -> SpreadElement,
    "ObjectLiteral" -> ObjectLiteral,
    "PropertyDefinitionList" -> PropertyDefinitionList,
    "PropertyDefinition" -> PropertyDefinition,
    "PropertyName" -> PropertyName,
    "LiteralPropertyName" -> LiteralPropertyName,
    "ComputedPropertyName" -> ComputedPropertyName,
    "CoverInitializedName" -> CoverInitializedName,
    "Initializer" -> Initializer,
    "TemplateLiteral" -> TemplateLiteral,
    "SubstitutionTemplate" -> SubstitutionTemplate,
    "TemplateSpans" -> TemplateSpans,
    "TemplateMiddleList" -> TemplateMiddleList,
    "MemberExpression" -> MemberExpression,
    "SuperProperty" -> SuperProperty,
    "MetaProperty" -> MetaProperty,
    "NewTarget" -> NewTarget,
    "ImportMeta" -> ImportMeta,
    "NewExpression" -> NewExpression,
    "CallExpression" -> CallExpression,
    "SuperCall" -> SuperCall,
    "ImportCall" -> ImportCall,
    "Arguments" -> Arguments,
    "ArgumentList" -> ArgumentList,
    "OptionalExpression" -> OptionalExpression,
    "OptionalChain" -> OptionalChain,
    "LeftHandSideExpression" -> LeftHandSideExpression,
    "CallMemberExpression" -> CallMemberExpression,
    "UpdateExpression" -> UpdateExpression,
    "UnaryExpression" -> UnaryExpression,
    "ExponentiationExpression" -> ExponentiationExpression,
    "MultiplicativeExpression" -> MultiplicativeExpression,
    "MultiplicativeOperator" -> MultiplicativeOperator,
    "AdditiveExpression" -> AdditiveExpression,
    "ShiftExpression" -> ShiftExpression,
    "RelationalExpression" -> RelationalExpression,
    "EqualityExpression" -> EqualityExpression,
    "BitwiseANDExpression" -> BitwiseANDExpression,
    "BitwiseXORExpression" -> BitwiseXORExpression,
    "BitwiseORExpression" -> BitwiseORExpression,
    "LogicalANDExpression" -> LogicalANDExpression,
    "LogicalORExpression" -> LogicalORExpression,
    "CoalesceExpression" -> CoalesceExpression,
    "CoalesceExpressionHead" -> CoalesceExpressionHead,
    "ShortCircuitExpression" -> ShortCircuitExpression,
    "ConditionalExpression" -> ConditionalExpression,
    "AssignmentExpression" -> AssignmentExpression,
    "AssignmentOperator" -> AssignmentOperator,
    "AssignmentPattern" -> AssignmentPattern,
    "ObjectAssignmentPattern" -> ObjectAssignmentPattern,
    "ArrayAssignmentPattern" -> ArrayAssignmentPattern,
    "AssignmentRestProperty" -> AssignmentRestProperty,
    "AssignmentPropertyList" -> AssignmentPropertyList,
    "AssignmentElementList" -> AssignmentElementList,
    "AssignmentElisionElement" -> AssignmentElisionElement,
    "AssignmentProperty" -> AssignmentProperty,
    "AssignmentElement" -> AssignmentElement,
    "AssignmentRestElement" -> AssignmentRestElement,
    "DestructuringAssignmentTarget" -> DestructuringAssignmentTarget,
    "Expression" -> Expression,
    "Statement" -> Statement,
    "Declaration" -> Declaration,
    "HoistableDeclaration" -> HoistableDeclaration,
    "BreakableStatement" -> BreakableStatement,
    "BlockStatement" -> BlockStatement,
    "Block" -> Block,
    "StatementList" -> StatementList,
    "StatementListItem" -> StatementListItem,
    "LexicalDeclaration" -> LexicalDeclaration,
    "LetOrConst" -> LetOrConst,
    "BindingList" -> BindingList,
    "LexicalBinding" -> LexicalBinding,
    "VariableStatement" -> VariableStatement,
    "VariableDeclarationList" -> VariableDeclarationList,
    "VariableDeclaration" -> VariableDeclaration,
    "BindingPattern" -> BindingPattern,
    "ObjectBindingPattern" -> ObjectBindingPattern,
    "ArrayBindingPattern" -> ArrayBindingPattern,
    "BindingRestProperty" -> BindingRestProperty,
    "BindingPropertyList" -> BindingPropertyList,
    "BindingElementList" -> BindingElementList,
    "BindingElisionElement" -> BindingElisionElement,
    "BindingProperty" -> BindingProperty,
    "BindingElement" -> BindingElement,
    "SingleNameBinding" -> SingleNameBinding,
    "BindingRestElement" -> BindingRestElement,
    "EmptyStatement" -> EmptyStatement,
    "ExpressionStatement" -> ExpressionStatement,
    "IfStatement" -> IfStatement,
    "IterationStatement" -> IterationStatement,
    "DoWhileStatement" -> DoWhileStatement,
    "WhileStatement" -> WhileStatement,
    "ForStatement" -> ForStatement,
    "ForInOfStatement" -> ForInOfStatement,
    "ForDeclaration" -> ForDeclaration,
    "ForBinding" -> ForBinding,
    "ContinueStatement" -> ContinueStatement,
    "BreakStatement" -> BreakStatement,
    "ReturnStatement" -> ReturnStatement,
    "WithStatement" -> WithStatement,
    "SwitchStatement" -> SwitchStatement,
    "CaseBlock" -> CaseBlock,
    "CaseClauses" -> CaseClauses,
    "CaseClause" -> CaseClause,
    "DefaultClause" -> DefaultClause,
    "LabelledStatement" -> LabelledStatement,
    "LabelledItem" -> LabelledItem,
    "ThrowStatement" -> ThrowStatement,
    "TryStatement" -> TryStatement,
    "Catch" -> Catch,
    "Finally" -> Finally,
    "CatchParameter" -> CatchParameter,
    "DebuggerStatement" -> DebuggerStatement,
    "UniqueFormalParameters" -> UniqueFormalParameters,
    "FormalParameters" -> FormalParameters,
    "FormalParameterList" -> FormalParameterList,
    "FunctionRestParameter" -> FunctionRestParameter,
    "FormalParameter" -> FormalParameter,
    "FunctionDeclaration" -> FunctionDeclaration,
    "FunctionExpression" -> FunctionExpression,
    "FunctionBody" -> FunctionBody,
    "FunctionStatementList" -> FunctionStatementList,
    "ArrowFunction" -> ArrowFunction,
    "ArrowParameters" -> ArrowParameters,
    "ConciseBody" -> ConciseBody,
    "ExpressionBody" -> ExpressionBody,
    "ArrowFormalParameters" -> ArrowFormalParameters,
    "MethodDefinition" -> MethodDefinition,
    "PropertySetParameterList" -> PropertySetParameterList,
    "GeneratorMethod" -> GeneratorMethod,
    "GeneratorDeclaration" -> GeneratorDeclaration,
    "GeneratorExpression" -> GeneratorExpression,
    "GeneratorBody" -> GeneratorBody,
    "YieldExpression" -> YieldExpression,
    "AsyncGeneratorMethod" -> AsyncGeneratorMethod,
    "AsyncGeneratorDeclaration" -> AsyncGeneratorDeclaration,
    "AsyncGeneratorExpression" -> AsyncGeneratorExpression,
    "AsyncGeneratorBody" -> AsyncGeneratorBody,
    "ClassDeclaration" -> ClassDeclaration,
    "ClassExpression" -> ClassExpression,
    "ClassTail" -> ClassTail,
    "ClassHeritage" -> ClassHeritage,
    "ClassBody" -> ClassBody,
    "ClassElementList" -> ClassElementList,
    "ClassElement" -> ClassElement,
    "AsyncFunctionDeclaration" -> AsyncFunctionDeclaration,
    "AsyncFunctionExpression" -> AsyncFunctionExpression,
    "AsyncMethod" -> AsyncMethod,
    "AsyncFunctionBody" -> AsyncFunctionBody,
    "AwaitExpression" -> AwaitExpression,
    "AsyncArrowFunction" -> AsyncArrowFunction,
    "AsyncConciseBody" -> AsyncConciseBody,
    "AsyncArrowBindingIdentifier" -> AsyncArrowBindingIdentifier,
    "CoverCallExpressionAndAsyncArrowHead" -> CoverCallExpressionAndAsyncArrowHead,
    "AsyncArrowHead" -> AsyncArrowHead,
    "Script" -> Script,
    "ScriptBody" -> ScriptBody,
    "Module" -> Module,
    "ModuleBody" -> ModuleBody,
    "ModuleItemList" -> ModuleItemList,
    "ModuleItem" -> ModuleItem,
    "ImportDeclaration" -> ImportDeclaration,
    "ImportClause" -> ImportClause,
    "ImportedDefaultBinding" -> ImportedDefaultBinding,
    "NameSpaceImport" -> NameSpaceImport,
    "NamedImports" -> NamedImports,
    "FromClause" -> FromClause,
    "ImportsList" -> ImportsList,
    "ImportSpecifier" -> ImportSpecifier,
    "ModuleSpecifier" -> ModuleSpecifier,
    "ImportedBinding" -> ImportedBinding,
    "ExportDeclaration" -> ExportDeclaration,
    "ExportFromClause" -> ExportFromClause,
    "NamedExports" -> NamedExports,
    "ExportsList" -> ExportsList,
    "ExportSpecifier" -> ExportSpecifier
  )
}
