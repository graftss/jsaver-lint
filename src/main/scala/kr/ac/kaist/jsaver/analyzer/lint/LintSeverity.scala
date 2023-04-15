package kr.ac.kaist.jsaver.analyzer.lint

trait LintSeverity

case object LintError extends LintSeverity
case object LintWarning extends LintSeverity

// a lint violation likely caused by analysis imprecision
case object LintImprecision extends LintSeverity