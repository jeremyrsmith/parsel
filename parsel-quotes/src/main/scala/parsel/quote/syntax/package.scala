package parsel.quote

import parsel.ast.Module
import parsel.parse.Parser
import parsel.parse.ParseError
import parsel.quote.macros.TreeReification

import scala.reflect.macros.{TypecheckException, whitebox}

package object syntax {

  implicit class PySyntax(val stringContext: StringContext) extends AnyVal {
    def py(splices: Any*): parsel.ast.Tree = macro PySyntaxImpl.lintNoSplices
    def pyq(splices: Any*): QuotedTree = macro PySyntaxImpl.lintSplice
  }


  private class PySyntaxImpl(override val c: whitebox.Context) extends TreeReification(c) {
    import c.universe._

    val quotableType = weakTypeOf[Quotable[Any]].typeConstructor

    private def parseOrAbort(code: String, pos: Position): parsel.ast.Tree = try Parser.parse(code) catch {
      case err@ParseError(msg, offset, line, col, lineStr) =>
        val outerOffset = pos.start + offset
        val errPos = c.enclosingPosition.withPoint(outerOffset).withStart(outerOffset).withEnd(outerOffset)
        c.abort(errPos, s"Syntax error: $msg")
    }

    def lintNoSplices(splices: Tree*): Tree = {
      if (splices.nonEmpty) {
        c.abort(c.enclosingPosition, "Splices not allowed in py interpolator; use pyq instead")
      } else {
        c.prefix.tree match {
          case Apply(_, List(Apply(_, List(codeTree@Literal(Constant(code: String)))))) =>
            val ast = parseOrAbort(code, codeTree.pos)
            reifyTree(ast)
          case _ => c.abort(c.enclosingPosition, "Unexpected tree: must be a string literal quoted with `py`")
        }
      }
    }

    def lintSplice(splices: c.Tree*): Tree = c.prefix.tree match {
      case Apply(_, List(Apply(_, stringPartsTrees))) =>
        val stringParts = stringPartsTrees.map {
          case Literal(Constant(s: String)) => s
          case tree => c.abort(tree.pos, "Expected a literal string here")
        }
        // We parse twice – the first one just to lint, and the second to get the AST with spliced names.
        // For the first one, we try to splice dummy identifiers that have the same range position as the splice.
        // This is so that error reporting of a parse error will report the correct position.
        val fileContent = c.prefix.tree.pos.source.content
        val spliceDummies = splices.map {
          case tree@Ident(name) =>
            if (tree.pos.start > 0 && tree.pos.start < fileContent.length) {
              "a" * (name.encodedName.toString.length - (fileContent(tree.pos.start - 1) match {
                case '{' => 3
                case _   => 1
              }))
            } else "a" * (name.encodedName.toString.length - 1)
          case tree =>
            // this will almost always be wrong, but it's the best we can do without range positions
            "a" * (showCode(tree).length - 3)
        }

        val dummyCode = stringParts.zipAll(spliceDummies, "", "").map { case (s, d) => s + d }.mkString
        parseOrAbort(dummyCode, stringPartsTrees.head.pos)

        def quotableInfo(tree: Tree): (parsel.ast.Name, Tree, Tree) = {
          val quotedName = freshName
          val quotable = try c.inferImplicitValue(appliedType(quotableType, tree.tpe.dealias.widen), silent = false) catch {
            case err: TypecheckException => c.abort(tree.pos, s"No implicit instance of Quotable[${showCode(tq"${tree.tpe}")}] is avaliable. Is an import missing?")
          }
          val nameCode = reifyTree(quotedName)
          (quotedName, nameCode, q"_root_.parsel.quote.QuotedValue($tree, $nameCode, $quotable)")
        }

        val stableSplices = splices.collect {
          case tree@Ident(_) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
            (tree.symbol, quotableInfo(tree))
        }.reverse.toMap

        val quotes = splices.map {
          case tree if stableSplices.contains(tree.symbol) => stableSplices(tree.symbol)
          case tree => quotableInfo(tree)
        }

        val code = stringParts.zipAll(quotes.map(_._1.name), "", "").map { case (s, d) => s + d}.mkString
        val ast = reifyTree(parseOrAbort(code, stringPartsTrees.head.pos))

        val quotedValuesMapElems = quotes.map(q => q._1 -> (q._2, q._3)).toMap.values.toList.map {
          case (nameCode, quotedValue) => q"($nameCode, $quotedValue)"
        }

        q"_root_.parsel.quote.QuotedTree($ast, _root_.scala.Predef.Map(..$quotedValuesMapElems))"
      case _ => c.abort(c.enclosingPosition, "Unexpected tree: must be a string literal quoted with `py`")
    }

  }
}
