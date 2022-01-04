package parsel
package parse

import ast.{Keyword => ASTKeyword, _}
import parsel.ast.Util.{Param, Params}
import parsel.parse.ExpressionParser.{named_expression, singleOrTuple, yield_expr}
import parsel.parse.Lexer._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Parser {

  def parse(input: String): Module = parse(new Lexer(input, ignoreWhitespace = true), input)

  def parse(lexer: Lexer, input: String): Module = {
    lexer.skip(Newline)
    val indentLevel = lexer.peek match {
      case Indent(n) => n
      case _         => 0
    }
    val rootParser = new BlockParser(indentLevel)
    try {
      val statements = rootParser.statements(lexer)
      while(lexer.hasNext && lexer.isEmptyLine) {
        lexer.skipLine()
      }
      if (lexer.nonEmpty && lexer.peek != EOF) {
        throw new Exception("parser stopped early!")
      }
      Module(statements)
    } catch {
      case err@Error(msg, offset) =>
        val nextNewline = input.indexOf('\n', offset) match {
          case -1 => input.length
          case n => n
        }
        var lineNumber = 1
        var lastLineOffset = 0
        var i = 0
        while (i < offset) {
          if (input.charAt(i) == '\n') {
            lineNumber += 1
            lastLineOffset = i
          }
          i += 1
        }
        val column = offset - lastLineOffset
        val detailErr = ParseError(msg, offset, lineNumber, column, input.substring(lastLineOffset, nextNewline))
        detailErr.setStackTrace(err.getStackTrace)
        throw detailErr
    }
  }

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
      stats.toSeq
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
              val lhs = lhss.head.withExprContext(Store)
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
              AnnAssign(lhs, typExpr, rhs, if (definitelyNonSimple || !lhs.isInstanceOf[Name]) 0 else 1)
            case _ =>
              // a simple assignment. Must have an rhs
              @tailrec def collect_targets(accum: Seq[Expr]): (Seq[Expr], Expr) = tokens.peek match {
                case Keyword("yield") => (accum, yield_expr(tokens))
                case Operator("=") =>
                  tokens.next()
                  collect_targets(accum :+ singleOrTuple(star_expressions(tokens)))
                case _ => (accum.dropRight(1), accum.last)
              }

              val (targets, rhs) = collect_targets(Seq(singleOrTuple(lhss)))
              Assign(targets.map(_.withExprContext(Store)), rhs, None)
          }

        case _ =>
          // is an aug-assign. Can't contain annotations or star target
          val lhs = t_primary(tokens)

          // this greedily took, but we have to go back and make sure it's not a function call
          lhs match {
            case Call(_, _, _) => throw Parser.Error("cannot assign to function call", pos)
            case lhs => lhs.withExprContext(Store)
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
          Alias(Name(name), Some(Name(alias)))
        case _ => Alias(Name(name), None)
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
        case _ => Some(Name(dotted_name(tokens)))
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
          Seq(Alias(Name("*"), None))
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
          Alias(Name(name), Some(Name(alias)))
        case _ => Alias(Name(name), None)
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
      Delete(del_targets(tokens).map(_.withExprContext(Del)))
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
              case FunctionDef(name, args, body, _, returns, typeComment) => AsyncFunctionDef(name, args, body, decorators.toSeq, returns, typeComment)
            }
            case _ => throw Parser.Error("expected def", tokens.currentOffset)
          }
        case Keyword("def")   => function_def(tokens).copy(decoratorList = decorators.toSeq)
        case Keyword("class") => class_def(tokens).copy(decoratorList = decorators.toSeq)
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
      FunctionDef(Name(name), args, body, Nil, annot, None)
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
      @tailrec def impl(accum: Params, hadDefault: Boolean, kwOnly: Boolean): Params = {
        val pos = tokens.currentOffset
        tokens.peek match {
          case Operator("**") =>
            tokens.next()
            val p = param_no_default(tokens)
            if (accum.kwParam.nonEmpty) {
              throw Parser.Error("a ** parameter was already specified", pos)
            }
            if (tokens.peek == Comma)
              tokens.next()
            accum.withKwParam(p)
          case Operator("/") =>
            if (accum.hasKws) {
              throw Parser.Error("Unexpected /", tokens.currentOffset)
            }
            tokens.next()
            if (tokens.peek == Comma)
              tokens.next()
            impl(accum.toPosOnly, hadDefault, kwOnly)
          case Operator("*") =>
            tokens.next()
            tokens.peek match {
              case Comma =>
                tokens.next()
                impl(accum, hadDefault, kwOnly = true)
              case _ =>
                if (accum.varArg.nonEmpty) {
                  throw Parser.Error("Invalid syntax", tokens.currentOffset)
                }
                val p = param_no_default(tokens)
                if (tokens.peek == Comma)
                  tokens.next()
                impl(accum.withVarArg(p), false, kwOnly = true)
            }
          case RParen => accum
          case _ =>
            val nextParam = param_maybe_default(tokens)
            if ((hadDefault || kwOnly) && nextParam.default.isEmpty) {
              throw Parser.Error("a required parameter can't follow a default or keyword parameter", pos)
            }
            val next = if (kwOnly)
              accum.withKwOnlyParam(nextParam)
            else
              accum.withParam(nextParam)

            if (tokens.peek == Comma)
              tokens.next()

            impl(next, hadDefault || nextParam.default.nonEmpty, kwOnly)
        }

      }
      val params = impl(Params.empty, false, false)
      Arguments.fromParams(params)
    }

    def param_maybe_default(tokens: Lexer): Param = {
      val p = param(tokens)
      tokens.peek match {
        case Operator("=") =>
          tokens.next()
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
          Param(Name(name), Some(annot), None)
        case _ => Param(Name(name), None, None)
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
      ClassDef(Name(name), args, kws, body, Nil)
    }

    def with_stmt(tokens: Lexer): With = {
      tokens.expect(Keyword("with"))

      val items = tokens.peek match {
        case LParen =>
          tokens.next()
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

        case _ => throw Parser.Error(s"expected except block or finally block", tokens.currentOffset)
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
              (Some(typ), Some(Name(name)))
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
      Global(names.toSeq)
    }

    def nonlocal_stmt(tokens: Lexer): Statement = {
      tokens.expect(Keyword("nonlocal"))
      val names = ArrayBuffer(ident(tokens))
      while (tokens.peek == Comma) {
        tokens.next()
        names += ident(tokens)
      }
      Nonlocal(names.toSeq)
    }

    def ident(tokens: Lexer): Name = {
      val Word(name) = tokens.expect(Word, "identifier")
      Name(name)
    }
  }

  object StringDecoder {
    def apply(content: String, delim: Quote, outerOffset: Int, flags: Option[String]): Either[JoinedStr, Constant[String]] = {
      val flagsLower = flags.map(_.toLowerCase).getOrElse("")
      val isRaw = flagsLower.contains('r')
      val isBytes = flagsLower.contains('b')
      val isFormatted = flagsLower.contains('f')
      val lexer = new Lexer(content, ignoreWhitespace = false, isQuotedExpr = true)
      lexer.skip(Indent)
      if (isFormatted) {
        Left(decodeFormatted(lexer, outerOffset, isRaw))
      } else {
        if (isBytes) {
          Right(Constant(BytesLiteral(decodeStringChunk(lexer, outerOffset, isRaw, isBytes, formatted = false), flagsLower)))
        } else {
          Right(Constant(StringLiteral(decodeStringChunk(lexer, outerOffset, isRaw, isBytes, formatted = false), flags)))
        }
      }
    }

    /**
      * Handles decoding string escapes
      * Does NOT handle `\N{UNICODE NAME}`-style escapes, because that would entail embedding the entire unicode database.
      */
    def decodeStringChunk(lexer: Lexer, outerOffset: Int, raw: Boolean, bytes: Boolean, formatted: Boolean): String = {
      val content = lexer.input
      @tailrec def impl(pos: Int, builder: StringBuilder): (String, Int) = {
        def parseHex(escapeChar: Char): (String, Int) = {
          val start = pos + 2
          val expectedLength = escapeChar match {
            case 'x' => 2
            case 'u' => 4
            case 'U' => 8
            case _ => throw new IllegalStateException("parseHex invoked with invalid escape char")
          }
          def err: Nothing = throw Parser.Error(s"truncated \\${escapeChar}${'X' * expectedLength} escape", outerOffset + start)

          val end = start + expectedLength

          if (content.length <= end)
            err

          val hexDigits = content.substring(pos + 2, end).toUpperCase

          if (hexDigits.exists(c => !c.isDigit && (c < 'A' || c > 'F')))
            err

          val char = new String(Character.toChars(Integer.parseInt(hexDigits)))
          (char, end)
        }

        if (content.length <= pos) {
          (builder.toString(), content.length)
        } else content.charAt(pos) match {
          case '\\' =>
            if (raw)
              impl(pos + 1, builder.append('\\'))
            else {
              if (content.length <= pos + 1) {
                throw Parser.Error("string literal cannot end with a '\\'", outerOffset + pos)
              }
              content.charAt(pos + 1) match {
                case '\n' => impl(pos + 2, builder)
                case '\\' => impl(pos + 2, builder.append('\\'))
                case '\'' => impl(pos + 2, builder.append('\''))
                case '\"' => impl(pos + 2, builder.append('\"'))
                case 'a'  => impl(pos + 2, builder.append(7.toChar))
                case 'b'  => impl(pos + 2, builder.append(8.toChar))
                case 'f'  => impl(pos + 2, builder.append(12.toChar))
                case 'n'  => impl(pos + 2, builder.append('\n'))
                case 'r'  => impl(pos + 2, builder.append('\r'))
                case 't'  => impl(pos + 2, builder.append('\t'))
                case 'v'  => impl(pos + 2, builder.append(11.toChar))
                case c@('x' | 'u' | 'U')  =>
                  val (str, nextPos) = parseHex(c)
                  impl(nextPos, builder.append(str))

                case d if d.isDigit && d <= '7' =>
                  // up to 3 octal digits
                  val octalStr = content.indexWhere(c => !c.isDigit || c > '7', pos + 2) match {
                    case -1 => d.toString
                    case n => content.substring(pos + 2, math.min(pos + 5, n))
                  }
                  val char = Integer.parseInt(octalStr, 8).toChar
                  impl(pos + 1 + octalStr.length, builder.append(char))

                case c =>
                  impl(pos + 2, builder.append('\\').append(c))
              }
            }
          case '{' if formatted && (content.length <= pos + 1 || content.charAt(pos + 1) != '{') =>
            (builder.mkString, pos) // stop here, it's a formatted expression
          case '}' if formatted && (content.length <= pos + 1 || content.charAt(pos + 1) != '}') =>
            (builder.mkString, pos)
          case '{' if formatted =>
            impl(pos + 2, builder.append("{")) // next character must be '{' as well, so continue
          case '}' if formatted =>
            impl(pos + 2, builder.append("}"))
          case c =>
            if (bytes) {
              if (c > 127)
                throw Parser.Error("bytes can only contain ASCII literal characters", pos)
              content.indexWhere(c => c == '\\' || c > 127, pos + 1) match {
                case -1 => (builder.append(content.substring(pos)).mkString, content.length)
                case n  => impl(n, builder.append(content.substring(pos, n)))
              }
            } else if (formatted) {
              content.indexWhere(c => c == '\\' || c == '{' || c == '}', pos + 1) match {
                case -1 => (builder.append(content.substring(pos)).mkString, content.length)
                case n  => impl(n, builder.append(content.substring(pos, n)))
              }
            } else {
              content.indexOf('\\', pos + 1) match {
                case -1 => (builder.append(content.substring(pos)).mkString, content.length)
                case n  => impl(n, builder.append(content.substring(pos, n)))
              }
            }
        }
      }
      val (result, nextOffs) = impl(lexer.currentOffset, new StringBuilder)
      lexer.unsafeSetOffset(nextOffs)
      result
    }

    def decodeFormatted(lexer: Lexer, outerOffset: Int, raw: Boolean): JoinedStr = {
      val content = lexer.input
      val flags = if (raw) Some("r") else None

      def parseQuotedExpr: Seq[Expr] = lexer.ignoringWhitespace {
        val pos = lexer.currentOffset
        if (lexer.peek == RBrace) {
          // NOTE: The f-string grammar in the lexical doc says this is allowed, but python throws an error.
          throw Parser.Error("f-string: empty expression not allowed", outerOffset + lexer.currentOffset)
        }

        def parseNextExpr = try ExpressionParser.star_expression_or_yield(lexer) catch {
          case err@Parser.Error(_, offset) =>
            val fixedOffset = err.copy(offset = offset + outerOffset)
            fixedOffset.setStackTrace(err.getStackTrace)
            throw fixedOffset
        }

        @tailrec def parseExprs(accum: Seq[Expr]): Expr = lexer.peek match {
          case RBrace | Colon | Operator("=" | "!") => singleOrTuple(accum)
          case Comma =>
            lexer.next()
            parseExprs(accum)
          case _ => parseExprs(accum :+ parseNextExpr)
        }

        val exprPos = lexer.currentOffset

        // single starred expression isn't allowed by python, even though grammar says it's OK.
        val expr = parseExprs(Seq(parseNextExpr)) match {
          case Starred(_, _) => throw Parser.Error("f-string: can't use starred expression here", outerOffset + exprPos)
          case expr => expr
        }

        val showExprString = lexer.peek match {
          case Operator("=") =>
            lexer.next()
            true
          case _ => false
        }

        val showStrEnd = lexer.currentOffset

        val conversion = lexer.peek match {
          case Operator("!") =>
            lexer.next()
            Some(lexer.expect(Word("s") | Word("r") | Word("a"), "conversion indicator (s | r | a)").word.head.toInt)
          case _ => None
        }

        val formatSpec = lexer.peek match {
          case Colon =>
            lexer.next()
            lexer.peek match {
              case RBrace => Some(JoinedStr(Nil))
              case other  => Some(impl(Seq.empty))
            }

          case _ => None
        }

        val fv = FormattedValue(expr, conversion, formatSpec)

        val result = if (showExprString) {
          Seq(Constant(StringLiteral(content.substring(pos, showStrEnd), None)), fv)
        } else {
          Seq(fv)
        }

        try {
          lexer.expect(RBrace)
          result
        } catch {
          case err@Parser.Error(_, _) =>
            val correctOffset = err.copy(offset = outerOffset + err.offset)
            correctOffset.setStackTrace(err.getStackTrace)
            throw correctOffset
        }
      }

      @tailrec def impl(pieces: Seq[Expr]): JoinedStr = if (!lexer.hasNext) {
        JoinedStr(pieces)
      } else lexer.peek match {
        case LBrace =>
          lexer.next()
          impl(pieces ++ parseQuotedExpr)
        case tok =>
          val chunk = decodeStringChunk(lexer, outerOffset, raw, bytes = false, formatted = true)
          val nextPieces = if (chunk.isEmpty) pieces else pieces.lastOption match {
            case Some(Constant(StringLiteral(chunk1, flags1))) =>
              pieces.dropRight(1) :+ Constant(StringLiteral(chunk1 + chunk, None))
            case _ => pieces :+ Constant(StringLiteral(chunk, flags))
          }

          if (!lexer.hasNext || lexer.peek == EOF || (chunk.isEmpty && tok == RBrace))
            JoinedStr(nextPieces)
          else
            impl(nextPieces)
      }

      impl(Seq.empty)
    }
  }
}