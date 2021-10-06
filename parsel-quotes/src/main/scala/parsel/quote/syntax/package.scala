package parsel.quote

import parsel.ast.{Module, Tree}
import parsel.parse.Parser
import parsel.parse.ParseError
import parsel.quote.macros.TreeReification

import scala.reflect.macros.whitebox

package object syntax {

  implicit class PySyntax(val stringContext: StringContext) extends AnyVal {
    def py(splices: Any*): Tree = macro PySyntaxImpl.lintNoSplices
    def pyq(splices: Any*): Tree = macro PySyntaxImpl.lintSplice
  }


  private class PySyntaxImpl(ctx: whitebox.Context) extends TreeReification(ctx) {
    import c.universe._

    def lintNoSplices(splices: c.universe.Tree*): Expr[Module] = {
      if (splices.nonEmpty) {
        c.abort(c.enclosingPosition, "Splices not allowed in py interpolator; use pyq instead")
      } else {
        c.prefix.tree match {
          case Apply(_, List(Apply(_, List(codeTree@Literal(Constant(code: String)))))) =>
            val ast = try Parser.parse(code) catch {
              case err@ParseError(msg, offset, line, col, lineStr) =>
                val errPos = c.enclosingPosition.withPoint(codeTree.pos.start + offset)
                c.abort(errPos, s"Syntax error: $msg")
            }
            c.Expr(reifyTree(c)(ast))
          case _ => c.abort(c.enclosingPosition, "Unexpected tree: must be a string literal quoted with `py`")
        }
      }
    }

    def lintSplice(splices: c.universe.Tree*): Expr[Module] = {
      val quotables = splices.map {
        tree =>

          c.inferImplicitValue(appliedType(weakTypeOf[Quotable[_]].typeConstructor, tree.tpe.dealias.widen), silent = false)
      }


      ???
    }
  }
}
