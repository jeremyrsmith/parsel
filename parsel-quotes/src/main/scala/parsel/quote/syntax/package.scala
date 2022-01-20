package parsel.quote

import macros.PySyntaxImpl
import parsel.ast.{CompleteTree, Module, Expr}

package object syntax {

  implicit class PySyntax(val stringContext: StringContext) extends AnyVal {
    /**
      * A Python quasiquote which eagerly splices quotable expressions, resulting in a [[Module]] containing the parsed statements
      */
    def py(splices: Any*): Module = macro PySyntaxImpl.statsEagerSplice

    /**
      * Same as [[py]], but strips a margin from the code like [[scala.collection.immutable.StringOps.stripMargin]] does
      */
    def pym(splices: Any*): Module = macro PySyntaxImpl.statsEagerSpliceMargin

    /**
      * A Python quasiquote which suspends quotable expressions, resulting in a [[QuotedTree]].
      */
    def pyq(splices: Any*): QuotedTree = macro PySyntaxImpl.statsLazySplice
    def pye(splices: Any*): Expr = macro PySyntaxImpl.exprEagerSplice
  }

}
