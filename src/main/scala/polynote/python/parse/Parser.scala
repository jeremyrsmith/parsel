package polynote.python
package parse

import ast.{Keyword => ASTKeyword, _}
import polynote.python.ast.Util.{KWOnlyParams, Param}
import polynote.python.parse.ExpressionParser.{named_expression, singleOrTuple, yield_expr}
import polynote.python.parse.Lexer._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Parser(input: String) {
  import Parser._
//  private var stack: List[State] = Block(0, ArrayBuffer.empty) :: Nil

  def parse(): Seq[Statement] = {
    val lexer = new Lexer(input, ignoreWhitespace = true)
    val rootParser = new BlockParser(0)
    try {
      rootParser.statements(lexer)
    } catch {
      case err: Throwable =>
        println(input.drop(lexer.currentOffset))
        throw err
    }
  }

}

object Parser {

  case class Error(msg: String, offset: Int) extends Throwable(s"$msg (pos $offset)")

  import ExpressionParser.{expression, star_targets, star_expressions, t_primary, arguments}

  class BlockParser(val indent: Int) extends AnyVal {
    private def expectIndent(tokens: Lexer): Unit = tokens.expect(Indent(indent))

    def statements(tokens: Lexer): Seq[Statement] = {
      @tailrec def impl(stats: Seq[Statement]): Seq[Statement] = tokens.peek match {
        case EOF => stats
        case _ if tokens.isEmptyLine =>
          tokens.skipLine()
          impl(stats)
        case Indent(`indent`) => impl(stats ++ statement(tokens))
        case _ => stats
      }

      impl(Seq.empty)
    }

    def statement(tokens: Lexer): Seq[Statement] = {
      if (tokens.isEmptyLine) {
        tokens.skipLine()
        return Nil
      }
      expectIndent(tokens)
      val stats = tokens.peek match {
        case Newline =>
          tokens.next()
          return Nil
        case Keyword(kw) => kw match {
          case "if" => Seq(if_stmt(tokens))
          case "def" => Seq(function_def(tokens))
          case "class" => Seq(class_def(tokens))
          case "with" => Seq(with_stmt(tokens))
          case "for" => Seq(for_stmt(tokens))
          case "try" => Seq(try_stmt(tokens))
          case "while" => Seq(while_stmt(tokens))
          case "async" => Seq(async_stmt(tokens))
          case _ => simple_stmt(tokens)
        }
        case Operator("@") => Seq(decorator_stmt(tokens))
        case _ => simple_stmt(tokens)
      }
      stats
    }

    def simple_stmt(tokens: Lexer): Seq[Statement] = {
      val stats = ArrayBuffer(small_stmt(tokens))
      while (tokens.peek == Semicolon) {
        tokens.next()
        tokens.peek match {
          case Newline | EOF =>
          case _ => stats += small_stmt(tokens)
        }
      }
      tokens.expect(Newline | EOF, "newline or EOF")
      stats
    }

    def small_stmt(tokens: Lexer): Statement = tokens.peek match {
      case Keyword(kw) => kw match {
        case "return" => return_stmt(tokens)
        case "import" => import_name(tokens)
        case "from" => import_from(tokens)
        case "raise" => raise_stmt(tokens)
        case "pass" =>
          tokens.next()
          Pass()
        case "del" => del_stmt(tokens)
        case "yield" => yield_stmt(tokens)
        case "assert" => assert_stmt(tokens)
        case "break" =>
          tokens.next()
          Break()
        case "continue" =>
          tokens.next()
          Continue()
        case "global" => global_stmt(tokens)
        case "nonlocal" => nonlocal_stmt(tokens)
        case _ => throw Parser.Error(s"Unknown keyword $kw", tokens.currentOffset)
      }
      case _ =>
        // must be either an assignment or an expression
        // we can detect an assignment by peeking at the root tokens of the line and searching for an assign or augassign operator
        val lineTokens = tokens.peekLineRootTokens
        lineTokens.indexWhere(isAssign) match {
          case -1 => ExprStatement(singleOrTuple(star_expressions(tokens))) // not an assignment, must be expression
          case assignPos => assignment(tokens, lineTokens, assignPos)
        }
    }

    private def assignment(tokens: Lexer, lineTokens: Seq[Token], assignPos: Int): Statement = {
      val definitelyNonSimple = tokens.peek == LParen
      val pos = tokens.currentOffset
      lineTokens(assignPos) match {
        case Operator("=") =>
          // not an aug-assign
          val lhss = star_targets(tokens)

          tokens.peek match {
            case Colon =>
              // it's an annotated assign. Can't have multiple targets or star target
              if (lhss.size != 1) {
                throw Parser.Error("only single target (not tuple) can be annotated", pos)
              }
              val lhs = lhss.head
              if (lhs.isInstanceOf[Starred]) {
                throw Parser.Error("invalid syntax", tokens.currentOffset)
              }

              tokens.next()
              val typExpr = expression(tokens)
              val rhs = tokens.peek match {
                case Operator("=") =>
                  tokens.next()
                  tokens.peek match {
                    case Keyword("yield") => Some(yield_expr(tokens))
                    case _ => Some(singleOrTuple(star_expressions(tokens)))
                  }
                case _ => None
              }
              AnnAssign(lhs, typExpr, rhs, if (definitelyNonSimple || !lhs.isInstanceOf[Ident]) 0 else 1)
            case _ =>
              // a simple assignment. Must have an rhs
              tokens.expect(Operator("="))

              @tailrec def collect_targets(accum: Seq[Expr]): (Seq[Expr], Expr) = tokens.peek match {
                case Keyword("yield") => (accum, yield_expr(tokens))
                case Newline | RParen | RBrace | RBracket | EOF => (accum.dropRight(1), accum.last)
                case _ => collect_targets(accum :+ singleOrTuple(star_expressions(tokens)))
              }

              val (targets, rhs) = collect_targets(Seq(singleOrTuple(lhss)))
              Assign(targets, rhs, None)
          }

        case _ =>
          // is an aug-assign. Can't contain annotations or star target
          val lhs = t_primary(tokens)

          // this greedily took, but we have to go back and make sure it's not a function call
          lhs match {
            case Call(_, _, _) => throw Parser.Error("cannot assign to function call", pos)
            case _ =>
          }

          val opPos = tokens.currentOffset
          val Operator(opChars) = tokens.expect(Operator)
          val op = ExpressionParser.operator(opChars.stripSuffix("="), opPos)

          val rhs = tokens.peek match {
            case Keyword("yield") => yield_expr(tokens)
            case _ => singleOrTuple(star_expressions(tokens))
          }

          AugAssign(lhs, op, rhs)
      }
    }

    private def isAssign(token: Token): Boolean = token match {
      case Operator("=" | "+=" | "-=" | "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=") => true
      case _ => false
    }


    def import_name(tokens: Lexer): Statement = {
      tokens.expect(Keyword("import"))
      Import(dotted_as_names(tokens))
    }

    def dotted_as_names(tokens: Lexer): Seq[Alias] = {
      val results = ArrayBuffer(dotted_as_name(tokens))
      while (tokens.peek == Comma) {
        tokens.next()
        results += dotted_as_name(tokens)
      }
      results.toSeq
    }

    def dotted_as_name(tokens: Lexer): Alias = {
      val name = dotted_name(tokens)
      tokens.peek match {
        case Keyword("as") =>
          tokens.next()
          val Word(alias) = tokens.expect(Word, "identifier")
          Alias(Ident(name), Some(Ident(alias)))
        case _ => Alias(Ident(name), None)
      }
    }

    def dotted_name(tokens: Lexer): String = {
      val Word(base) = tokens.expect(Word, "identifier")
      val result = new StringBuilder(base)
      while (tokens.peek == Dot) {
        tokens.next()
        val Word(next) = tokens.expect(Word, "identifier")
        result.append(".")
        result.append(next)
      }
      result.toString
    }

    def import_from(tokens: Lexer): Statement = {
      tokens.expect(Keyword("from"))
      var level = 0
      while (tokens.peek == Dot) {
        tokens.next()
        level += 1
      }

      val module = tokens.peek match {
        case Keyword("import") =>
          if (level == 0) {
            throw Parser.Error("invalid syntax", tokens.currentOffset)
          }
          None
        case _ => Some(Ident(dotted_name(tokens)))
      }

      tokens.expect(Keyword("import"))

      ImportFrom(module, import_from_as_names(tokens), Some(level).filterNot(_ == 0))
    }

    def import_from_as_names(tokens: Lexer): Seq[Alias] = {
      tokens.peek match {
        case LParen =>
          tokens.next()
          val result = import_from_as_names(tokens)
          tokens.expect(RParen)
          result
        case Operator("*") =>
          Seq(Alias(Ident("*"), None))
        case _ =>
          val aliases = ArrayBuffer(import_from_as_name(tokens))
          while (tokens.peek == Comma) {
            tokens.next()
            tokens.peek match {
              case Newline | RParen | EOF => return aliases.toSeq
              case _ => aliases += import_from_as_name(tokens)
            }
          }
          aliases.toSeq
      }
    }

    def import_from_as_name(tokens: Lexer): Alias = {
      val Word(name) = tokens.expect(Word, "identifier")
      tokens.peek match {
        case Keyword("as") =>
          tokens.next()
          val Word(alias) = tokens.expect(Word, "alias")
          Alias(Ident(name), Some(Ident(alias)))
        case _ => Alias(Ident(name), None)
      }
    }

    def raise_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("raise"))
      tokens.peek match {
        case Newline | Semicolon | EOF | RParen | RBrace | RBracket => Raise(None, None)
        case _ =>
          val ex = expression(tokens)
          tokens.peek match {
            case Keyword("from") =>
              Raise(Some(ex), Some(expression(tokens)))
            case _ => Raise(Some(ex), None)
          }
      }
    }

    def del_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("del"))
      Delete(del_targets(tokens))
    }

    def del_targets(tokens: Lexer): Seq[Expr] = {
      val targets = ArrayBuffer(del_target(tokens))
      while (tokens.peek == Comma) {
        tokens.next()
        tokens.peek match {
          case Newline | Semicolon | RParen | RBrace | RBracket => return targets.toSeq
          case _ => targets += del_target(tokens)
        }
      }
      targets.toSeq
    }

    def del_target(tokens: Lexer): Expr = {
      val pos = tokens.currentOffset
      t_primary(tokens) match {
        case Call(_, _, _) => throw Parser.Error("cannot delete function call", pos)
        case other => other
      }
    }

    def decorator_stmt(tokens: Lexer): Statement = {
      def nextDecorator = {
        tokens.expect(Operator("@"))
        val d = named_expression(tokens)
        tokens.expect(Newline)
        expectIndent(tokens)
        d
      }

      val decorators = ArrayBuffer(nextDecorator)
      while (tokens.peek == Operator("@")) {
        decorators += nextDecorator
      }

      tokens.peek match {
        case Keyword("async") =>
          tokens.next()
          tokens.peek match {
            case Keyword("def") => function_def(tokens) match {
              case FunctionDef(name, args, body, _, returns, typeComment) => AsyncFunctionDef(name, args, body, decorators, returns, typeComment)
            }
            case _ => throw Parser.Error("expected def", tokens.currentOffset)
          }
        case Keyword("def")   => function_def(tokens).copy(decoratorList = decorators)
        case Keyword("class") => class_def(tokens).copy(decoratorList = decorators)
        case _ => throw Parser.Error("expected class or def after decorators", tokens.currentOffset)
      }
    }

    def async_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("async"))
      tokens.peek match {
        case Keyword("for") => for_stmt(tokens) match {
          case For(target, iter, body, orElse, typeComment) => AsyncFor(target, iter, body, orElse, typeComment)
        }
        case Keyword("with") => with_stmt(tokens) match {
          case With(items, body, typeComment) => AsyncWith(items, body, typeComment)
        }
        case Keyword("def") => function_def(tokens) match {
          case FunctionDef(name, args, body, decoratorList, returns, typeComment) => AsyncFunctionDef(name, args, body, decoratorList, returns, typeComment)
        }
        case _ => throw Parser.Error("expected for, with, or def after async", tokens.currentOffset)
      }
    }

    def function_def(tokens: Lexer): FunctionDef = {
      tokens.expect(Keyword("def"))
      val Word(name) = tokens.expect(Word, "identifier")
      tokens.expect(LParen)
      val args = parameters(tokens)
      if (tokens.peek == Comma)
        tokens.next()
      tokens.expect(RParen)

      val annot = tokens.peek match {
        case Operator("->") =>
          tokens.next()
          Some(expression(tokens))
        case _ => None
      }

      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      FunctionDef(Ident(name), args, body, Nil, annot, None)
    }

    @tailrec private def block(tokens: Lexer): Seq[Statement] = tokens.peek match {
      case Indent(n) if n > indent =>
        new BlockParser(n).statements(tokens)
      case _ if tokens.isEmptyLine =>
        tokens.skipLine()
        block(tokens)
      case _ => throw Parser.Error("expected block", tokens.currentOffset)
    }

    def parameters(tokens: Lexer): Arguments = {
      @tailrec def impl(accum: (Seq[Param], Seq[Param], KWOnlyParams), hadDefault: Boolean, kwOnly: Boolean): (Seq[Param], Seq[Param], KWOnlyParams) = {
        val (posOnlyParams, accumParams, kwParams) = accum
        val pos = tokens.currentOffset
        tokens.peek match {
          case Operator("**") =>
            tokens.next()
            val p = param_no_default(tokens)
            if (kwParams.kwParam.nonEmpty) {
              throw Parser.Error("a ** parameter was already specified", pos)
            }
            if (tokens.peek == Comma)
              tokens.next()
            (posOnlyParams, accumParams, kwParams.copy(kwParam = Some(p)))
          case Operator("/") =>
            if (posOnlyParams.nonEmpty || kwParams.nonEmpty || hadDefault) {
              throw Parser.Error("Unexpected /", tokens.currentOffset)
            }
            tokens.next()
            if (tokens.peek == Comma)
              tokens.next()
            impl((accumParams, Seq.empty, kwParams), hadDefault, kwOnly)
          case Operator("*") =>
            tokens.next()
            tokens.peek match {
              case Comma =>
                tokens.next()
                impl(accum, hadDefault, kwOnly = true)
              case _ =>
                if (kwParams.vararg.nonEmpty) {
                  throw Parser.Error("Invalid syntax", tokens.currentOffset)
                }
                val p = param_no_default(tokens)
                if (tokens.peek == Comma)
                  tokens.next()
                impl((posOnlyParams, accumParams, kwParams.copy(vararg = Some(p))), false, kwOnly = true)
            }
          case RParen => accum
          case _ =>
            val nextParam = param_maybe_default(tokens)
            if ((hadDefault || kwOnly) && nextParam.default.isEmpty) {
              throw Parser.Error("a required parameter can't follow a default or keyword parameter", pos)
            }
            val next = if (kwOnly)
              (posOnlyParams, accumParams, kwParams.copy(kwOnlyParams = kwParams.kwOnlyParams :+ nextParam))
            else
              (posOnlyParams, accumParams :+ nextParam, kwParams)

            if (tokens.peek == Comma)
              tokens.next()

            impl(next, hadDefault || nextParam.default.nonEmpty, kwOnly)
        }

      }
      val (posOnly, posArgs, kwArgs) = impl((Seq.empty, Seq.empty, KWOnlyParams(None, Seq.empty, None)), false, false)
      Arguments.fromParams(posOnly, posArgs, Option(kwArgs).filter(_.nonEmpty))
    }

    def param_maybe_default(tokens: Lexer): Param = {
      val p = param(tokens)
      tokens.peek match {
        case Operator("=") =>
          val default = expression(tokens)
          p.copy(default = Some(default))
        case _ => p
      }
    }

    def param_no_default(tokens: Lexer): Param = {
      val p = param(tokens)
      tokens.peek match {
        case Operator("=") => throw Parser.Error("default not allowed here", tokens.currentOffset)
        case _ => p
      }
    }

    def param(tokens: Lexer): Param = {
      val Word(name) = tokens.expect(Word, "identifier")
      tokens.peek match {
        case Colon =>
          tokens.next()
          val annot = expression(tokens)
          Param(Ident(name), Some(annot), None)
        case _ => Param(Ident(name), None, None)
      }
    }

    def if_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("if"))
      val test = named_expression(tokens)
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      collect_elifs(tokens, If(test, body, Nil))
    }

    private def collect_elifs(tokens: Lexer, ifStat: If): If = tokens.peekN(2) match {
      case Seq(Indent(`indent`), Keyword("elif")) =>
        tokens.next()
        tokens.next()
        val test = named_expression(tokens)
        tokens.expect(Colon)
        tokens.expect(Newline)
        val body = block(tokens)
        ifStat.copy(orElse = Seq(collect_elifs(tokens, If(test, body, Nil))))
      case Seq(Indent(`indent`), Keyword("else")) =>
        tokens.next()
        tokens.next()
        tokens.expect(Colon)
        tokens.expect(Newline)
        val body = block(tokens)
        ifStat.copy(orElse = body)
      case _ => ifStat
    }

    def class_def(tokens: Lexer): ClassDef = {
      tokens.expect(Keyword("class"))
      val Word(name) = tokens.expect(Word, "class name")
      val (args, kws) = tokens.peek match {
        case LParen =>
          tokens.next()
          val args = arguments(tokens)
          tokens.expect(RParen)
          args
        case _ => (Nil, Nil)
      }
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      ClassDef(Ident(name), args, kws, body, Nil)
    }

    def with_stmt(tokens: Lexer): With = {
      tokens.expect(Keyword("with"))
      val items = tokens.peek match {
        case LParen =>
          val items = ArrayBuffer(with_item(tokens))
          while (tokens.peek == Comma) {
            tokens.next()
            if (tokens.peek != RParen)
              items += with_item(tokens)
          }
          tokens.expect(RParen)
          items.toSeq
        case _ =>
          val items = ArrayBuffer(with_item(tokens))
          while (tokens.peek == Comma) {
            tokens.next()
            items += with_item(tokens)
          }
          items.toSeq
      }
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      With(items, body, None)
    }

    def with_item(tokens: Lexer): WithItem = {
      val expr = expression(tokens)
      tokens.peek match {
        case Keyword("as") =>
          tokens.next()
          WithItem(expr, Some(expression(tokens)))
        case _ => WithItem(expr, None)
      }
    }


    def for_stmt(tokens: Lexer): For = {
      tokens.expect(Keyword("for"))
      val targets = star_targets(tokens)
      tokens.expect(Keyword("in"))
      val inExprs = star_expressions(tokens)
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      val elseBlock = maybe_else_block(tokens).toSeq.flatten
      For(singleOrTuple(targets), singleOrTuple(inExprs), body, elseBlock, None)
    }

    def maybe_block(tokens: Lexer, Opener: Token): Option[Seq[Statement]] = tokens.peekN(2) match {
      case Seq(Indent(`indent`), Opener) =>
        tokens.next()
        tokens.next()
        tokens.expect(Colon)
        tokens.expect(Newline)
        Some(block(tokens))
      case _ => None
    }

    def maybe_else_block(tokens: Lexer): Option[Seq[Statement]] = maybe_block(tokens, Keyword("else"))
    def maybe_finally_block(tokens: Lexer): Option[Seq[Statement]] = maybe_block(tokens, Keyword("finally"))

    def try_stmt(tokens: Lexer): Try = {
      tokens.expect(Keyword("try"))
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      expectIndent(tokens)
      tokens.peek match {
        case Keyword("except") =>
          val handlers = except_blocks(tokens, except_blocks(tokens, Seq(except_block(tokens))))
          val elseBody = maybe_else_block(tokens).toSeq.flatten
          val finallyBody = maybe_finally_block(tokens).toSeq.flatten
          Try(body, handlers, elseBody, finallyBody)

        case Keyword("finally") =>
          tokens.next()
          tokens.expect(Colon)
          tokens.expect(Newline)
          val finallyBody = block(tokens)
          Try(body, Nil, Nil, finallyBody)
      }
    }

    @tailrec private def except_blocks(tokens: Lexer, accum: Seq[ExceptHandler]): Seq[ExceptHandler] = tokens.peekN(2) match {
      case Seq(Indent(`indent`), Keyword("except")) =>
        tokens.next()
        except_blocks(tokens, accum :+ except_block(tokens))
      case _ => accum
    }

    def except_block(tokens: Lexer): ExceptHandler = {
      tokens.expect(Keyword("except"))
      val (typ, asName) = tokens.peek match {
        case Colon => (None, None)
        case _ =>
          val typ = expression(tokens)
          tokens.peek match {
            case Keyword("as") =>
              tokens.next()
              val Word(name) = tokens.expect(Word, "name")
              (Some(typ), Some(Ident(name)))
            case _ => (Some(typ), None)
          }
      }
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      ExceptHandler(typ, asName, body)
    }

    def while_stmt(tokens: Lexer): While = {
      tokens.expect(Keyword("while"))
      val test = named_expression(tokens)
      tokens.expect(Colon)
      tokens.expect(Newline)
      val body = block(tokens)
      val elseBody = maybe_else_block(tokens).toSeq.flatten
      While(test, body, elseBody)
    }

    def return_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("return"))
      tokens.peek match {
        case Newline | EOF | Semicolon | RParen | RBrace | RBracket => Return(None)
        case _ => Return(Some(singleOrTuple(star_expressions(tokens))))
      }
    }

    def yield_stmt(tokens: Lexer): Statement = ExprStatement(yield_expr(tokens))

    def assert_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("assert"))
      val expr = expression(tokens)
      tokens.peek match {
        case Comma =>
          tokens.next()
          Assert(expr, Some(expression(tokens)))
        case _ => Assert(expr, None)
      }
    }

    def global_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("global"))
      val names = ArrayBuffer(ident(tokens))
      while (tokens.peek == Comma) {
        tokens.next()
        names += ident(tokens)
      }
      Global(names)
    }

    def nonlocal_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("nonlocal"))
      val names = ArrayBuffer(ident(tokens))
      while (tokens.peek == Comma) {
        tokens.next()
        names += ident(tokens)
      }
      Nonlocal(names)
    }

    def ident(tokens: Lexer): Ident = {
      val Word(name) = tokens.expect(Word, "identifier")
      Ident(name)
    }
  }
}