package parsel
package parse

import ast._
import parsel.ast.Util.{Param, Params}
import parsel.parse.Parser.Error

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object ExpressionParser {
  import Lexer._

  val ctx: ExprContext = ExprContext.load

  /**
    * Parse a Python expression from the given input string
    * @throws ParseError if the input does not begin with a valid Python expression, or if there is leftover input
    *                    after parsing an expression
    */
  def parse(input: String): Expr = {
    val (result, remainder) = parseS(input)
    if (remainder.nonEmpty)
      parseError(input, "Remaining input after parsing; expected EOF", input.length - remainder.length, None)
    result
  }

  /**
    * Parse a Python expression from the given input string, and return the parsed expression along with the remaining
    * input
    * @throws ParseError if the input does not begin with a valid Python expression
    * @return a tuple of the parsed expression, along with the remaining input after parsing an expression (which may
    *         be empty)
    */
  def parseS(input: String): (Expr, String) = {
    val lexer = new Lexer(input, ignoreWhitespace = true)
    val expr = parse(lexer, input)
    (expr, if (lexer.currentOffset < input.length) input.substring(lexer.currentOffset) else "")
  }

  /**
    * Parse given input as an expression, using given lexer.
    * @throws ParseError if the input does not begin with a valid Python expression
    */
  private def parse(lexer: Lexer, input: String): Expr = {
    lexer.skip(Indent)
    try {
      val expr = expression(lexer)
      while(lexer.hasNext && lexer.isEmptyLine) {
        lexer.skipLine()
      }
      expr
    } catch {
      case err@Error(msg, offset) => parseError(input, msg, offset, Some(err))
    }
  }

  private def parseError(input: String, msg: String, offset: Int, err: Option[Throwable]): Nothing = {
    val (lineNumber, column, linePart) = posInfo(input, offset)
    val detailErr = ParseError(msg, offset, lineNumber, column, linePart)
    err.foreach(err => detailErr.setStackTrace(err.getStackTrace))
    throw detailErr
  }

  private def posInfo(input: String, offset: Int): (Int, Int, String) = {
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
    (lineNumber, column, input.substring(lastLineOffset, nextNewline))
  }

  def star_expressions(tokens: Lexer): Seq[Expr] = {
    val results = ArrayBuffer(star_expression(tokens))
    while (tokens.peek == Comma) {
      tokens.next()
      tokens.peek match {
        case Operator("*") => results += star_expression(tokens)
        case Newline | RParen | RBrace | RBracket | Operator(_) =>
        case _ => results += star_expression(tokens)
      }
    }
    results.toSeq
  }

  def star_expression(tokens: Lexer): Expr = tokens.peek match {
    case Operator("*") =>
      tokens.next()
      Starred(bitwise_or(tokens), ctx)
    case _ => expression(tokens)
  }

  def star_expression_or_yield(tokens: Lexer): Expr = tokens.peek match {
    case Keyword("yield") => yield_expr(tokens)
    case Operator("*")    =>
      tokens.next()
      Starred(bitwise_or(tokens), ctx)
    case _ => expression(tokens)
  }

  def star_named_expression(tokens: Lexer): Expr = tokens.peek match {
    case Operator("*") =>
      tokens.next()
      Starred(bitwise_or(tokens), ctx)
    case _ => named_expression(tokens)
  }

  def named_expression(tokens: Lexer): Expr = tokens.peekN(2) match {
    case Seq(Word(name), ColonEquals) =>
      tokens.next()
      tokens.next()
      NamedExpr(Name(name), expression(tokens))
    case _ => expression(tokens)
  }

  def expression(tokens: Lexer): Expr = {
    tokens.peek match {
      case Keyword("lambda") => lambdef(tokens)
      case _ =>
        val d = disjunction(tokens)
        tokens.peek match {
          case Keyword("if") =>
            tokens.next()
            val body1 = expression(tokens)
            tokens.expect(Keyword("else"))
            val body2 = expression(tokens)
            IfExp(d, body1, body2)
          case _ => d
        }
    }
  }

  def lambdef(tokens: Lexer): Expr = {
    tokens.expect(Keyword("lambda"))
    val args = tokens.peek match {
      case Colon => Arguments.empty
      case _ => lambda_params(tokens)
    }
    tokens.expect(Colon)
    val body = expression(tokens)
    Lambda(args, body)
  }


  def lambda_params(tokens: Lexer): Arguments = {
    @tailrec def impl(accum: Params, hadDefault: Boolean, kwOnly: Boolean): Params = {
      tokens.peek match {
        case Operator("**") =>
          tokens.next()
          val Word(name) = tokens.expect(Word, "identifier")
          if (tokens.peek == Comma)
            tokens.next()
          accum.withKwParam(name)
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
              val Word(id) = tokens.expect(Word, "identifier")
              if (tokens.peek == Comma)
                tokens.next()
              impl(accum.withVarArg(id), false, kwOnly = true)
          }
        case Colon => accum
        case _ =>
          val nextParam = lambda_param(tokens, !hadDefault || kwOnly)
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


  def lambda_param(tokens: Lexer, allowPosOnly: Boolean = true): Param = {
    val Word(name) = tokens.expect(Word, "identifier")
    tokens.peek match {
      case Operator("=") =>
        tokens.next()
        Param(Name(name), None, Some(expression(tokens)))
      case _ if allowPosOnly => Param(Name(name), None, None)
      case _ => throw Parser.Error("Position-only argument can't follow a keyword argument", tokens.currentOffset)
    }
  }

  def disjunction(tokens: Lexer): Expr = {
    val next = conjunction(tokens)

    def isOr = tokens.peek match {
      case Keyword("or") | Operator("||") => true
      case _ => false
    }

    if (isOr) {
      tokens.next()
      val operands = ArrayBuffer(next, conjunction((tokens)))
      while (isOr) {
        tokens.next()
        operands += conjunction(tokens)
      }
      BoolOp(Or, operands.toSeq)
    } else next
  }

  def conjunction(tokens: Lexer): Expr = {
    val next = inversion(tokens)

    def isAnd = tokens.peek match {
      case Keyword("and") => true
      case _ => false
    }

    if (isAnd) {
      tokens.next()
      val operands = ArrayBuffer(next, inversion(tokens))
      while (isAnd) {
        tokens.next()
        operands += inversion(tokens)
      }
      BoolOp(And, operands.toSeq)
    } else next
  }

  def inversion(tokens: Lexer): Expr = {
    var count = 0
    while (tokens.peek == Keyword("not")) {
      count += 1
      tokens.next()
    }

    var expr = comparison(tokens)
    while (count > 0) {
      count -= 1
      expr = UnaryOp(Not, expr)
    }
    expr
  }

  def comparison(tokens: Lexer): Expr = {
    def isNextComparison = tokens.peekN(2) match {
      case Seq(Operator("==" | "!=" | "<=" | "<" | ">=" | ">"), _) => true
      case Seq(Keyword("not"), Keyword("in")) => true
      case Seq(Keyword("is"), _) => true
      case Seq(Keyword("in"), _) => true
      case _ => false
    }

    val left = bitwise_or(tokens)
    val comparators = ArrayBuffer.empty[Expr]
    val compareOps = ArrayBuffer.empty[ComparisonOperator]
    while (isNextComparison) {
      tokens.next() match {
        case Operator(chars@("==" | "!=" | "<=" | "<" | ">=" | ">")) =>
          compareOps += (chars match {
            case "==" => Eq
            case "!=" => NotEq
            case "<=" => LtE
            case "<" => Lt
            case ">=" => GtE
            case ">" => Gt
          })
          comparators += bitwise_or(tokens)
        case Keyword("not") =>
          tokens.expect(Keyword("in"))
          compareOps += NotIn
          comparators += bitwise_or(tokens)
        case Keyword("is") =>
          tokens.peek match {
            case Keyword("not") =>
              tokens.next()
              compareOps += IsNot
              comparators += bitwise_or(tokens)
            case _ =>
              compareOps += Is
              comparators += bitwise_or(tokens)
          }
        case Keyword("in") =>
          compareOps += In
          comparators += bitwise_or(tokens)
        case _ =>
          throw Parser.Error("expected comparison operator", tokens.currentOffset)
      }
    }
    if (compareOps.isEmpty) {
     left
    } else {
      Compare(left, compareOps.toSeq, comparators.toSeq)
    }
  }

  def bitwise_or(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator("|") =>
        tokens.next()
        impl(BinOp(lhs, BitOr, bitwise_xor(tokens)))
      case _ => lhs
    }
    impl(bitwise_xor(tokens))
  }

  def bitwise_xor(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator("^") =>
        tokens.next()
        impl(BinOp(lhs, BitXor, bitwise_and(tokens)))
      case _ => lhs
    }
    impl(bitwise_and(tokens))
  }

  def bitwise_and(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator("&") =>
        tokens.next()
        impl(BinOp(lhs, BitAnd, shift_expr(tokens)))
      case _ => lhs
    }
    impl(shift_expr(tokens))
  }

  def shift_expr(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator(chars@("<<" | ">>")) =>
        tokens.next()
        impl(BinOp(lhs, operator(chars, tokens.currentOffset), sum(tokens)))
      case _ => lhs
    }
    impl(sum(tokens))
  }

  def sum(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator(chars@("+" | "-")) =>
        tokens.next()
        impl(BinOp(lhs, operator(chars, tokens.currentOffset), term(tokens)))
      case other =>
        lhs
    }
    impl(term(tokens))
  }

  def term(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator(chars@("*" | "/" | "//" | "%" | "@")) =>
        tokens.next()
        impl(BinOp(lhs, operator(chars, tokens.currentOffset), factor(tokens)))
      case _ => lhs
    }
    impl(factor(tokens))
  }

  def factor(tokens: Lexer): Expr = {
    @tailrec def unaryOps(accum: List[UnaryOperator]): List[UnaryOperator] = tokens.peek match {
      case Operator(chars@("+" | "-" | "~")) =>
        tokens.next()
        val op = chars match {
          case "+" => UAdd
          case "-" => USub
          case "~" => Invert
        }
        unaryOps(op :: accum)
      case _ => accum
    }
    unaryOps(Nil).foldLeft(power(tokens)) {
      (expr, op) => UnaryOp(op, expr)
    }
  }

  def power(tokens: Lexer): Expr = {
    @tailrec def impl(lhs: Expr): Expr = tokens.peek match {
      case Operator("**") =>
        tokens.next()
        impl(BinOp(lhs, Pow, factor(tokens)))
      case _ => lhs
    }
    impl(await_primary(tokens))
  }

  def await_primary(tokens: Lexer): Expr = tokens.peek match {
    case Keyword("await") =>
      tokens.next()
      Await(primary(tokens))
    case _ => primary(tokens)
  }

  def operator(chars: String, pos: Int): ast.Operator = chars match {
    case "|"  => BitOr
    case "&"  => BitAnd
    case "^"  => BitXor
    case "<<" => LShift
    case ">>" => RShift
    case "+"  => Add
    case "-"  => Sub
    case "*"  => Mult
    case "/"  => Div
    case "//" => FloorDiv
    case "%"  => Mod
    case "@"  => MatMult
    case other => throw Parser.Error(s"Unknown operator $other", pos)
  }

  private object StringFlags {
    def unapply(str: String): Boolean = str match {
      case "r" | "u" | "R" | "U" | "f" | "F" | "fr" | "Fr" | "fR" | "FR" | "rf" | "rF" | "Rf" | "RF" => true
      case _ => false
    }
  }

  private object ByteFlags {
    def unapply(str: String): Boolean = str match {
      case "b" | "B" | "br" | "Br" | "bR" | "BR" | "rb" | "rB" | "Rb" | "RB" => true
      case _ => false
    }
  }

  private def stringOrIdent(tokens: Lexer, word: String): Expr = word match {
    case StringFlags() | ByteFlags() =>
      tokens.peek match {
        case q@Quote(_) =>
          tokens.next()
          strings(tokens, q, Some(word))
        case _ => Name(word)
      }
    case ident => primary1(tokens, Name(ident))
  }

  private def strings(tokens: Lexer, Delim: Quote, flags: Option[String]): Expr = {
    @tailrec def impl(Delim: Quote, flags: Option[String], accum: Either[JoinedStr, Constant[String]]): Expr = {
      // we're inside a string which started with Delim and flags (byte flags on the left, string flags on the right).
      // We've previously seen accum string literals.
      // Find the end of this string literal, and see if there's another one after it
      val startPos = tokens.currentOffset
      def nextStart = tokens.peekN(2) match {
        case Seq(Word(flags@(StringFlags() | ByteFlags())), Quote(_)) =>
          tokens.next()
          val nextDelim = tokens.expect(Quote)
          Some((nextDelim, Some(flags)))
        case Seq(Quote(_), _) =>
          val nextDelim = tokens.expect(Quote)
          Some((nextDelim, None))
        case _ =>
          None
      }

      def collapseJoined(left: Seq[Expr], right: Seq[Expr]) = (left, right) match {
        case (prev :+ Constant(StringLiteral(value1, _)), Constant(StringLiteral(value2, _)) +: rest) =>
          val combined = value1 + value2
          if (combined.isEmpty)
            prev ++ rest
          else
            prev ++ (Constant(StringLiteral(combined, None)) +: rest)
        case _ => left ++ right
      }

      def combine(next: Either[JoinedStr, Constant[String]]): Either[JoinedStr, Constant[String]] = (accum, next) match {
        case (accum, Right(Constant(StringLiteral("", _) | BytesLiteral("", _)))) => accum
        case (Right(Constant(StringLiteral(value1, flags))), Right(Constant(StringLiteral(value2, _)))) =>
          Right(Constant(StringLiteral(value1 + value2, flags)))
        case (Right(Constant(BytesLiteral(value1, flags))), Right(Constant(BytesLiteral(value2, _)))) =>
          Right(Constant(BytesLiteral(value1 + value2, flags)))
        case (Right(_), Right(_)) |
             (Left(_), Right(Constant(BytesLiteral(_, _)))) |
             (Right(Constant(BytesLiteral(_, _))), Left(_)) =>
          throw Parser.Error("cannot mix bytes and nonbytes literals", startPos)
        case (Left(joinedStr), Right(c)) =>
          Left(JoinedStr(collapseJoined(joinedStr.values, Seq(c).filterNot(_.value.value.isEmpty))))
        case (Right(c), Left(joinedStr)) => Left(JoinedStr(collapseJoined(Seq(c).filterNot(_.value.value.isEmpty), joinedStr.values)))
        case (Left(joined1), Left(joined2)) => Left(JoinedStr(collapseJoined(joined1.values, joined2.values)))
      }

      tokens.next() match {
        case Delim =>
          // it ended without any content
          nextStart match {
            case Some((nextDelim, nextFlags)) =>
              val expr = if (flags.exists(_.toLowerCase.contains("f")))
                Left(JoinedStr(Nil))
              else nextFlags match {
                case Some(flags@ByteFlags()) => Right(Constant(BytesLiteral("", flags)))
                case _ => Right(Constant(StringLiteral("", flags)))
              }

              impl(nextDelim, nextFlags, combine(expr))
            case None => accum.fold[Expr](identity, identity)
          }
        case Word(str) =>
          tokens.expect(Delim)
          val decoded = combine(Parser.StringDecoder(str, Delim, startPos, flags))
          nextStart match {
            case Some((nextDelim, nextFlags)) => impl(nextDelim, nextFlags, decoded)
            case None => decoded.fold[Expr](identity, identity)
          }
        case tok => throw Parser.Error(s"Unexpected token '${tok.value}'", tokens.currentOffset)
      }
    }
    val start = flags match {
      case Some(flags@ByteFlags()) => Right(Constant(BytesLiteral("", flags)))
      case Some(flags@StringFlags()) if flags.toLowerCase.contains("f") => Left(JoinedStr(Nil))
      case _ => Right(Constant(StringLiteral("", flags)))
    }
    impl(Delim, flags, start)
  }

  def primary(tokens: Lexer): Expr = primary1(tokens, atom(tokens))

  @tailrec private def primary1(tokens: Lexer, startingWith: Expr): Expr = tokens.peek match {
    case Dot =>
      tokens.next()
      val name = tokens.expect(Word, "identifier")
      primary1(tokens, Attribute(startingWith, Name(name.value), ctx))
    case LParen =>
      val result = genexpOrArgs(tokens) match {
        case Left((posArgs, kwArgs)) => Call(startingWith, posArgs, kwArgs)
        case Right(genExp) => Call(startingWith, Seq(genExp), Nil)
      }
      result
    case LBracket =>
      val ss = slices(tokens)
      primary1(tokens, Subscript(startingWith, singleOrTuple(ss), ctx))
    case other =>
      startingWith
  }

  private def slices(tokens: Lexer): Seq[Slice] = {
    tokens.expect(LBracket)
    val slices = ArrayBuffer(slice(tokens))
    while (tokens.peek != RBracket) {
      tokens.expect(Comma)
      slices += slice(tokens)
    }
    tokens.expect(RBracket)
    slices.toSeq
  }

  private def slice(tokens: Lexer): Slice = {
    def parseUpper(lower: Option[Expr]): Slice = tokens.peek match {
      case RBracket | Comma => Slice(lower, None, None)
      case _            =>
        tokens.expect(Colon);
        tokens.peek match {
          case RBracket | Comma => Slice(lower, None, None)
          case Colon =>
            tokens.next()
            tokens.skip(Colon)
            parseStep(lower, None)
          case _ =>
            val upper = expression(tokens)
            tokens.skip(Colon)
            parseStep(lower, Some(upper))
        }
    }

    def parseStep(lower: Option[Expr], upper: Option[Expr]): Slice = tokens.peek match {
      case RBracket | Comma => Slice(lower, upper, None)
      case _ => Slice(lower, upper, Some(expression(tokens)))
    }

    tokens.peek match {
      case Comma => Slice(None, None, None)
      case Colon => parseUpper(None)
      case _ => parseUpper(Some(expression(tokens)))
    }
  }


  def atom(tokens: Lexer): Expr = maybeAtom(tokens).getOrElse {
    throw Parser.Error("Expected atom", tokens.currentOffset)
  }

  private def maybeAtom(tokens: Lexer): Option[Expr] = tokens.peek match {
    case Word(word) =>
      tokens.next()
      Some(stringOrIdent(tokens, word))
    case Keyword(word@("True" | "False" | "None")) =>
      tokens.next()
      val const = word match {
        case "True" => Constant(BooleanLiteral(true))
        case "False" => Constant(BooleanLiteral(false))
        case "None" => Constant(NoneLiteral)
      }
      Some(const)
    case q@Quote(_) =>
      tokens.next()
      Some(strings(tokens, q, None))
    case num: Num[_] =>
      tokens.next()
      Some(Constant(num.toLiteral))
    case LParen => Some(tupleOrGroupOrGenExp(tokens))
    case LBracket => Some(listOrListComp(tokens))
    case LBrace => Some(dictOrSetOrDictCompOrSetComp(tokens))
    case Dot =>
      tokens.expect(Dot, "...")
      tokens.expect(Dot, "...")
      tokens.expect(Dot, "...")
      Some(Constant(Ellipsis))
    case _ => None
  }

  def tupleOrGroupOrGenExp(tokens: Lexer): Expr = {

    def body: Expr = {
      val result = tokens.peek match {
        case Keyword("yield") => yield_expr(tokens)
        case _ =>
          val loc = tokens.currentOffset
          val first = star_named_expression(tokens)
          tokens.peek match {
            case Keyword("for" | "async") =>
              if (first.isInstanceOf[Starred])
                throw Parser.Error("iterable unpacking cannot be used in comprehension", loc)
              genexpFrom(tokens, first)
            case _ =>
              val exprs = ArrayBuffer[Expr](first)
              while (tokens.peek == Comma) {
                tokens.next()
                if (tokens.peek != RParen) {
                  exprs += star_named_expression(tokens)
                }
              }
              singleOrTuple(exprs.toSeq)
          }
      }
      result
    }

    tokens.expect(LParen)
    val result = tokens.peek match {
      case RParen =>
        ConstructTuple(Nil, ctx)
      case Comma =>
        tokens.next()
        tokens.peek match {
          case RParen => ConstructTuple(Nil, ctx)
          case _ => body
        }
      case _ => body
    }
    tokens.expect(RParen)
    result
  }

  def listOrListComp(tokens: Lexer): Expr = {
    def body = {
      val loc = tokens.currentOffset
      val first = star_named_expression(tokens)
      val result = tokens.peek match {
        case Keyword("for" | "async") =>
          if (first.isInstanceOf[Starred]) {
            throw Parser.Error("iterable unpacking cannot be used in comprehension", loc)
          }
          val generators = for_if_clauses(tokens)
          ListComp(first, generators)
        case _ =>
          val results = ArrayBuffer(first)
          while (tokens.peek == Comma) {
            tokens.next()
            if (tokens.peek != RParen) {
              results += star_named_expression(tokens)
            }
          }
          ConstructList(results.toSeq, ctx)
      }
      tokens.expect(RBracket)
      result
    }
    tokens.expect(LBracket)
    tokens.peek match {
      case RBracket =>
        tokens.next()
        ConstructList(Nil, ctx)
      case Comma =>
        tokens.next()
        tokens.peek match {
          case RBracket =>
            tokens.next()
            ConstructList(Nil, ctx)
          case _ => body
        }
      case _ =>
        body
    }
  }

  def dictOrSetOrDictCompOrSetComp(tokens: Lexer): Expr = {
    tokens.expect(LBrace)
    val result = tokens.peek match {
      case RBrace =>
        Dict(Nil, Nil)
      case Comma => throw Parser.Error("invalid syntax", tokens.currentOffset)
      case Operator("**") => dict_body(tokens) // must be a dict
      case _ =>
        val loc = tokens.currentOffset
        val first = star_named_expression(tokens)
        tokens.peek match {
          case Colon => // dict or dict comp
            // TODO: assert that first isn't starred or named
            tokens.next()
            val firstValue = expression(tokens)
            tokens.peek match {
              case Keyword("for" | "async") => // dict comp
                val comps = for_if_clauses(tokens)
                DictComp(first, firstValue, comps)
              case Comma =>
                tokens.next()
                val (restKeys, restValues) = double_starred_kvpairs(tokens).unzip
                Dict(first +: restKeys, firstValue +: restValues)
              case RBrace =>
                Dict(Seq(first), Seq(firstValue))
              case _ => throw Parser.Error("expected }", tokens.currentOffset)
            }
          case Comma => // a set
            val items = ArrayBuffer(first)
            while (tokens.peek == Comma) {
              tokens.next()
              if (tokens.peek != RBrace)
                items += star_named_expression(tokens)
            }
            ConstructSet(items.toSeq)

          case Keyword("for" | "async") => // a set comprehension
            val comps = for_if_clauses(tokens)
            if (first.isInstanceOf[Starred]) {
              throw Parser.Error("iterable unpacking cannot be used in comprehension", loc)
            }
            SetComp(first, comps)
          case RBrace => ConstructSet(Seq(first))
          case _ => throw Parser.Error("expected }", tokens.currentOffset)
        }
    }
    tokens.expect(RBrace)
    result
  }

  def dict_body(tokens: Lexer): Expr = {
    val (keys, values) = double_starred_kvpairs(tokens).unzip
    Dict(keys, values)
  }

  def double_starred_kvpairs(tokens: Lexer): Seq[(Expr, Expr)] = {
    val pairs = ArrayBuffer(double_starred_kvpair(tokens))
    while (tokens.peek == Comma) {
      tokens.next()
      if (tokens.peek != RBrace)
        pairs += double_starred_kvpair(tokens)
    }
    pairs.toSeq
  }

  def double_starred_kvpair(tokens: Lexer): (Expr, Expr) = tokens.peek match {
    case Operator("**") =>
      tokens.next()
      (Constant(NoneLiteral), bitwise_or(tokens))
    case _ =>
      val key = expression(tokens)
      tokens.expect(Colon)
      val value = expression(tokens)
      (key, value)
  }

  def genexpOrArgs(tokens: Lexer): Either[(Seq[Expr], Seq[ast.Keyword]), GeneratorExp] = {
    tokens.expect(LParen)
    val result = tokens.peek match {
      case RParen =>
        Left((Nil, Nil))
      case Operator("*") | Operator("**") =>
        // must be args, genexp doesn't allow this
        Left(args_inner(tokens, (Seq.empty, Seq.empty)))
      case _ =>
        val expr = named_expression(tokens)
        tokens.peek match {
          case RParen =>
            Left((Seq(expr), Nil))
          case Comma =>
            tokens.next()
            Left(args_inner(tokens, (Seq(expr), Nil)))
          case Operator("=") =>
            tokens.next()
            expr match {
              case id@Name(_) => Left(args_inner(tokens, (Nil, Seq(ast.Keyword(Some(id), expression(tokens))))))
              case _ => throw Parser.Error("Unexpected =", tokens.currentOffset)
            }
          case Keyword("for") =>
            Right(genexpFrom(tokens, expr))
          case other => throw Parser.Error(s"Unexpected token ${other.value}", tokens.currentOffset)
        }
    }
    tokens.expect(RParen)
    result
  }

  def arguments(tokens: Lexer): (Seq[Expr], Seq[ast.Keyword]) = args_inner(tokens, (Seq.empty, Seq.empty))

  @tailrec private def args_inner(tokens: Lexer, accum: (Seq[Expr], Seq[ast.Keyword])): (Seq[Expr], Seq[ast.Keyword]) = {
    val (accumExpr, accumKw) = accum
    tokens.peek match {
      case RParen => accum
      case Operator("**") =>
        tokens.next()
        (accumExpr, accumKw :+ ast.Keyword(None, expression(tokens)))
      case Comma =>
        tokens.next()
        args_inner(tokens, accum)
      case other =>
        val arg = other match {
          case Operator("*") =>
            tokens.next()
            expression(tokens)
          case _ => expression(tokens)
        }
        tokens.peek match {
          case Comma =>
            tokens.next()
            args_inner(tokens, (accumExpr :+ arg, accumKw))
          case Operator("=") =>
            tokens.next()
            arg match {
              case id@Name(_) => args_inner(tokens, (accumExpr, accumKw :+ ast.Keyword(Some(id), expression(tokens))))
              case _ => throw Parser.Error("Unexpected =", tokens.currentOffset)
            }
          case RParen => (accumExpr :+ arg, accumKw)
          case other => throw Parser.Error("Invalid syntax", tokens.currentOffset)
        }
    }
  }

  def genexp(tokens: Lexer): GeneratorExp = {
    tokens.expect(LParen)
    val result = genexp_inner(tokens)
    tokens.expect(RParen)
    result
  }

  def genexp_inner(tokens: Lexer): GeneratorExp = {
    val expr = named_expression(tokens)
    genexpFrom(tokens, expr)
  }

  def genexpFrom(tokens: Lexer, expr: Expr): GeneratorExp = {
    val generators = for_if_clauses(tokens)
    GeneratorExp(expr, generators)
  }

  def for_if_clauses(tokens: Lexer): Seq[Comprehension] = {
    val results = ArrayBuffer(for_if_clause(tokens))
    while (tokens.peek == Keyword("for") || tokens.peek == Keyword("async")) {
      results += for_if_clause(tokens)
    }
    results.toSeq
  }

  def for_if_clause(tokens: Lexer): Comprehension = {
    def impl(async: Boolean): Comprehension = {
      tokens.expect(Keyword("for"), "for")
      val targets = star_targets(tokens)
      tokens.expect(Keyword("in"), "in")
      val inExpr = disjunction(tokens)
      val ifs = new ArrayBuffer[Expr]
      while (tokens.peek == Keyword("if")) {
        tokens.next()
        ifs += disjunction(tokens)
      }
      Comprehension(singleOrTuple(targets), inExpr, ifs.toSeq, if (async) 1 else 0)
    }
    tokens.peek match {
      case Keyword("async") =>
        tokens.next()
        impl(true)
      case _ => impl(false)
    }
  }

  def yield_expr(tokens: Lexer): Expr = {
    tokens.expect(Keyword("yield"))
    tokens.peek match {
      case Keyword("from") =>
        tokens.next()
        YieldFrom(expression(tokens))
      case Newline | RParen | RBrace | RBracket => Yield(None)
      case other => Yield(Some(expression(tokens)))
    }
  }

  def star_targets(tokens: Lexer): Seq[Expr] = {
    @tailrec def impl(accum: Seq[Expr]): Seq[Expr] = {
      val next = star_target(tokens)
      tokens.peek match {
        case Comma =>
          tokens.next()
          tokens.peek match {
            case Operator("=") | Keyword("in") => accum
            case _ => impl(accum :+ next)
          }
        case _ => accum :+ next
      }
    }
    impl(Seq.empty)
  }

  @tailrec def star_target(tokens: Lexer): Expr = tokens.peek match {
    case Operator("*") =>
      tokens.next()
      star_target(tokens)
    case _ => target_with_star_atom(tokens)
  }

  def target_with_star_atom(tokens: Lexer): Expr = tokens.peek match {
    case LParen | LBracket => star_atom(tokens)
    case _ => t_primary(tokens)
  }

  @tailrec def star_targets_list_seq(tokens: Lexer, accum: Seq[Expr]): Seq[Expr] = tokens.peek match {
    case RParen => accum
    case _ =>
      val accum1 = accum :+ star_target(tokens)
      tokens.peek match {
        case Comma =>
          tokens.next()
          tokens.peek match {
            case RBracket => accum1
            case _ => star_targets_list_seq(tokens, accum1)
          }
        case _ => accum1
      }
  }

  @tailrec def star_targets_tuple_seq(tokens: Lexer, accum: Seq[Expr]): Seq[Expr] = tokens.peek match {
    case RParen => accum
    case _ =>
      val accum1 = accum :+ star_target(tokens)
      tokens.peek match {
        case Comma =>
          tokens.next()
          tokens.peek match {
            case RParen => accum1
            case _ => star_targets_tuple_seq(tokens, accum1)
          }
        case _ => accum1
      }
  }

  def star_atom(tokens: Lexer): Expr = tokens.peek match {
    case LParen =>
      tokens.next()
      tokens.peek match {
        case RParen =>
          tokens.next()
          ConstructTuple(Nil, ctx)
        case _ =>
          val result = singleOrTuple(star_targets_tuple_seq(tokens, Seq.empty))
          tokens.expect(RParen)
          result
      }
    case LBracket =>
      tokens.next()
      tokens.peek match {
        case RBracket =>
          tokens.next()
          ConstructList(Nil, ctx)
        case _ =>
          val elems = star_targets_list_seq(tokens, Seq.empty)
          tokens.expect(RBracket)
          ConstructList(elems, ctx)
      }
    case Word(id) => Name(id)
    case _ => throw Parser.Error("expected ( | [ | identifier", tokens.currentOffset)
  }

  def single_target(tokens: Lexer): Expr = tokens.peek match {
    case LParen =>
      tokens.next()
      val result = single_target(tokens)
      tokens.expect(RParen)
      result
    case _ => t_primary(tokens)
  }

  def t_primary(tokens: Lexer): Expr = t_primary1(tokens, atom(tokens))
  @tailrec private def t_primary1(tokens: Lexer, startingWith: Expr): Expr = tokens.peek match {
    case Dot =>
      tokens.next()
      val Word(name) = tokens.expect(Word)
      t_primary1(tokens, Attribute(startingWith, Name(name), ctx))
    case LBracket => t_primary1(tokens, Subscript(startingWith, singleOrTuple(slices(tokens)), ctx))
    case LParen =>
      val result = genexpOrArgs(tokens) match {
        case Left((posArgs, kwArgs)) => Call(startingWith, posArgs, kwArgs)
        case Right(genExp) => Call(startingWith, Seq(genExp), Nil)
      }
      result
    case _ => startingWith
  }

  private def t_primary_lookahead(tokens: Lexer): Boolean = tokens.peek match {
    case LParen | LBracket | Dot => true
    case _ => false
  }

  def singleOrTuple(exprs: Seq[Expr]): Expr = exprs match {
    case Seq(expr) => expr
    case exprs     => ConstructTuple(exprs, ctx)
  }

}
