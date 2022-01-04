package parsel.quote

import macros.PySyntaxImpl
import parsel.ast.CompleteTree

package object syntax {

  implicit class PySyntax(val stringContext: StringContext) extends AnyVal {
    def py(splices: Any*): CompleteTree = macro PySyntaxImpl.lintNoSplices
    def pyq(splices: Any*): QuotedTree = macro PySyntaxImpl.lintSplice
  }

}
