package parsel.quote.macros

import parsel.parse.{ExpressionParser, ParseError, Parser}
import parsel.quote.{Quotable, freshName}

import java.util.concurrent.atomic.AtomicInteger
import scala.reflect.macros.{TypecheckException, whitebox}


private[quote] class PySyntaxImpl(override val c: whitebox.Context) extends TreeReification(c) {
  import c.universe._

  private val quotableType = weakTypeOf[Quotable[Any]].typeConstructor

  private def parseOrAbort[T <: parsel.ast.Tree](parseCode: => T, pos: Position): T = try parseCode catch {
    case err@ParseError(msg, offset, line, col, lineStr) =>
      val outerOffset = pos.start + offset
      val errPos = c.enclosingPosition.withPoint(outerOffset).withStart(outerOffset).withEnd(outerOffset)
      c.abort(errPos, s"Syntax error: $msg")
  }

  private def parseStatsOrAbort(code: String, pos: Position): parsel.ast.Module = parseOrAbort(Parser.parse(code), pos)
  private def parseExprOrAbort(code: String, pos: Position): parsel.ast.Expr = parseOrAbort(ExpressionParser.parse(code), pos)

  def statsEagerSplice(splices: Tree*): Tree = splice(splices, false, parseStatsOrAbort)
  def statsLazySplice(splices: c.Tree*): Tree = splice(splices, true, parseStatsOrAbort)
  def exprEagerSplice(splices: c.Tree*): Tree = splice(splices, false, parseExprOrAbort)

  private def splice(splices: Seq[c.Tree], suspend: Boolean, parser: (String, Position) => parsel.ast.Tree): Tree = c.prefix.tree match {
    case Apply(_, List(Apply(_, stringPartsTrees))) =>
      val stringParts = stringPartsTrees.map {
        case Literal(Constant(s: String)) => s
        case tree => c.abort(tree.pos, "Expected a literal string here")
      }
      // We parse twice – the first one just to lint, and the second to get the AST with spliced names.
      // For the first one, we try to splice dummy identifiers that have the same range position as the splice.
      // This is so that error reporting of a parse error will report the correct position.
      val fileContent = c.prefix.tree.pos.source.content

      val parts = stringParts.zipAll(splices, "", EmptyTree)

      val dummyCode = parts.map {
        case (strPart, tree) =>
          val len = tree match {
            case tree@Ident(name) if tree.pos.start > 0 && tree.pos.start < fileContent.length && fileContent(tree.pos.start - 1) == '{' =>
              name.encodedName.toString.length - 3
            case Ident(name) =>
              name.encodedName.toString.length - 1
            case tree =>
              // this will almost always be wrong, but it's the best we can do without range positions
              showCode(tree).length - 3
          }
          strPart + ("a" * len)
      }.mkString

      parser(dummyCode, stringPartsTrees.head.pos)

      def quotableInfo(tree: Tree): (parsel.ast.Name, Tree, Tree) = {
        val quotedName = freshName
        val quotable = try c.inferImplicitValue(appliedType(quotableType, tree.tpe.dealias.widen), silent = false) catch {
          case err: TypecheckException => c.abort(tree.pos, s"No implicit instance of Quotable[${showCode(tq"${tree.tpe}")}] is avaliable. Is an import missing?")
        }
        val nameCode = reifyTree(quotedName)
        if (suspend)
          (quotedName, nameCode, q"_root_.parsel.quote.QuotedValue($tree, $nameCode, $quotable)")
        else
          (quotedName, nameCode, q"${quotable}.quote($tree)")
      }

      val stableSplices = splices.collect {
        case tree@Ident(_) if !isQuotedTrees(tree) && tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
          (tree.symbol, quotableInfo(tree))
      }.reverse.toMap

      // these placeholders are for splices of AST trees (or Seqs of them)
      val splicedTreePlaceholders = new scala.collection.mutable.HashMap[String, Tree]

      val quotes = splices.map {
        case tree if stableSplices.contains(tree.symbol) => stableSplices(tree.symbol)
        case tree if !isQuotedTrees(tree) => quotableInfo(tree)
        case quotedTrees =>
          val placeholder = freshPlaceholder
          splicedTreePlaceholders.put(placeholder.name, quotedTrees)
          (placeholder, reifyTree(placeholder), EmptyTree)
      }

      val code = stringParts.zipAll(quotes.map(_._1.name), "", "").map { case (s, d) => s + d}.mkString

      val treeSplicer = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case q"Seq(_root_.parsel.ast.ExprStatement(_root_.parsel.ast.Name(${Literal(Constant(name: String))})))" if splicedTreePlaceholders.contains(name) =>
            q"_root_.parsel.ast.Tree.asStats(${splicedTreePlaceholders(name)})"
          case q"_root_.parsel.ast.ExprStatement(_root_.parsel.ast.Name(${Literal(Constant(name: String))}))" if splicedTreePlaceholders.contains(name) =>
            splicedTreePlaceholders(name)
          case q"Seq(_root_.parsel.ast.Name(${Literal(Constant(name: String))}))" if splicedTreePlaceholders.contains(name) =>
            q"_root_.parsel.ast.Tree.asExprs(${splicedTreePlaceholders(name)})"
          case q"_root_.parsel.ast.Name(${Literal(Constant(name: String))})" if splicedTreePlaceholders.contains(name) =>
            splicedTreePlaceholders(name)
          case tree => super.transform(tree)
        }
      }

      val ast = treeSplicer.transform(reifyTree(parser(code, stringPartsTrees.head.pos)))

      val quotedNames = quotes.filterNot(_._3 == EmptyTree)
        .map(q => q._1.name -> (q._2, q._3))
        .toMap

      if (suspend) {
        val quotedValuesMapElems = quotedNames.values.toList.map {
          case (nameCode, quotedValue) => q"($nameCode, $quotedValue)"
        }

        q"_root_.parsel.quote.QuotedTree($ast, _root_.scala.Predef.Map(..$quotedValuesMapElems))"
      } else {
        val valueSplicer = new Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"_root_.parsel.ast.Name(${Literal(Constant(name: String))})" if quotedNames.contains(name) =>
              quotedNames(name)._2
            case _ => super.transform(tree)
          }
        }
        valueSplicer.transform(ast)
      }

    case _ => c.abort(c.enclosingPosition, "Unexpected tree: must be a string literal quoted with `py`")
  }

  private def isQuotedTrees(tree: Tree): Boolean = isSingleTree(tree) || isMultipleTrees(tree)

  private def isSingleTree(tree: Tree): Boolean = tree.tpe.dealias <:< weakTypeOf[parsel.ast.Tree]
  private def isMultipleTrees(tree: Tree): Boolean = tree.tpe.dealias <:< weakTypeOf[Seq[parsel.ast.Tree]]

  private val nextPlaceholder = new AtomicInteger()
  private def freshPlaceholder: parsel.ast.Name = {
    val id = nextPlaceholder.getAndIncrement()
    parsel.ast.Name(s"__PARSEL_PLACEHOLDER_${id}__")
  }
}