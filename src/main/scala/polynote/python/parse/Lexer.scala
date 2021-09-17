package polynote.python.parse

import polynote.python.ast.NumericLiteral
import polynote.python.parse.Lexer.Token

import scala.annotation.tailrec
import scala.reflect.ClassTag

class Lexer(input: String, ignoreWhitespace: Boolean = false) extends Iterator[Token] {
  import Lexer._
  private var offset = 0
  private var stack: List[State] = Root :: Nil
  // TODO: lookahead memo buffer - if we fill/drain a lookahead buffer, we could avoid re-lexing a lot of stuff on peeks

  override def hasNext: Boolean = offset <= input.length

  @tailrec private def peekNext(offset: Int = this.offset, stack: List[State] = this.stack, skipWs: Boolean = ignoreWhitespace): (Token, Int, List[State]) =
    if (offset >= input.length)
      (EOF, input.length, End :: Nil)
    else stack match {
      case Nil => Lexer.parseError(input, offset, "Empty lexer stack")
      case End :: Nil => (EOF, input.length + 1, stack)
      case h :: t =>
        val tup@(tok, nextOffs, nextStack) = h.matchNext(input, offset, stack)
        tok match {
          case Whitespace | Comment(_) if skipWs => peekNext(nextOffs, nextStack, skipWs)
          case _ => tup
        }
    }

  @tailrec private def peekNextN(n: Int, toks: Seq[Token], offset: Int, stack: List[State]): Seq[Token] = if (offset >= input.length) toks else {
    if (n == 0) {
      toks
    } else {
      stack match {
        case Nil => Lexer.parseError(input, offset, "Empty lexer stack")
        case End :: Nil => toks :+ EOF
        case h :: t =>
          val (nextTok, nextOffs, nextStack) = h.matchNext(input, offset, stack)
          nextTok match {
            case Whitespace if ignoreWhitespace => peekNextN(n, toks, nextOffs, nextStack)
            case other => peekNextN(n - 1, toks :+ other, nextOffs, nextStack)
          }
      }
    }
  }

  def peek: Token = peekNext()._1

  def peekN(n: Int): Seq[Token] = if (n <= 0) Seq.empty[Token] else {
    peekNextN(n, Seq.empty, offset, stack)
  }

  def peekLineRootTokens: Seq[Token] = {
    @tailrec def impl(accum: Seq[Token], offset: Int, stack: List[State]): Seq[Token] = stack match {
      case Nil => Lexer.parseError(input, offset, "Empty lexer stack")
      case End :: _ => accum
      case h :: t =>
        val (nextTok, nextOffs, nextStack) = h.matchNext(input, offset, stack)
        nextStack match {
          case (Root | End) :: _ if h == InLine && (nextTok != Whitespace || !ignoreWhitespace) && (nextTok != Newline) => accum :+ nextTok
          case (Root | End) :: _ => accum
          case InLine :: _ if (h == Root || h == InLine) && (nextTok != Whitespace || !ignoreWhitespace) =>
            impl(accum :+ nextTok, nextOffs, nextStack)
          case _ => impl(accum, nextOffs, nextStack)
        }
    }
    impl(Seq.empty, offset, stack)
  }

  def isEmptyLine: Boolean = {
    @tailrec def impl(offset: Int, stack: List[State]): Boolean = stack match {
      case Nil => Lexer.parseError(input, offset, "Empty lexer stack")
      case End :: _ => true
      case h :: t =>
        val (nextTok, nextOffs, nextStack) = h.matchNext(input, offset, stack)
        nextTok match {
          case Indent(_) | Whitespace | Comment(_) => impl(nextOffs, nextStack)
          case Newline | EOF => true
          case _ => false
        }
    }
    impl(offset, stack)
  }

  def skipLine(): Unit = {
    var tok = next()
    while (tok != Newline && tok != EOF) {
      tok = next()
    }
  }

  def skip(tok: Token): Unit = {
    if (peek == tok)
      next()
    ()
  }

  def expect[T <: Token](matcher: TokenMatcher[T], as: String): T = {
    val tok = next()
    if (matcher.matches(tok))
      tok.asInstanceOf[T]
    else
      Lexer.parseError(input, currentOffset - tok.value.length, s"Expected $as; found ${tok.value}")
  }

  def expect[T <: Token](matcher: TokenMatcher[T]): Token = expect(matcher, matcher.toString)

  def currentOffset: Int = offset

  def skipWhile(pf: PartialFunction[Token, Boolean]): Unit = {
    while (hasNext) {
      val (tok, nextOffset, nextStack) = peekNext()
      if (!pf.applyOrElse[Token, Boolean](tok, _ => false))
        return
      offset = nextOffset
      stack = nextStack
    }
  }

  def skipWhitespace(): Unit = {
    while (hasNext) {
      val (tok, nextOffset, nextStack) = peekNext(skipWs = false)
      tok match {
        case Whitespace | Comment(_) =>
          offset = nextOffset
          stack = nextStack
        case _ => return
      }
    }
  }

  override def next(): Token = {
    if (ignoreWhitespace) {
      skipWhitespace()
    }
    if (offset >= input.length) {
      stack = End :: Nil
      offset = input.length + 1
      EOF
    } else {
      val result = stack match {
        case Nil => Lexer.parseError(input, offset, "Empty lexer stack")
        case End :: Nil => EOF
        case h :: t =>
          val (tok, nextOffset, nextStack) = h.matchNext(input, offset, stack)
          offset = nextOffset
          stack = nextStack
          tok
      }
      if (ignoreWhitespace && result != Newline) {
        skipWhitespace()
      }
      result
    }
  }
}

object Lexer {

  private def parseError(str: String, pos: Int, msg: String): Nothing = {
    val part = str.take(pos)
    val lines = part.linesWithSeparators.toSeq
    throw ParseError(msg, lines.size, lines.last.length, pos)
  }

  private trait State {
    def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State])
  }

  private def isOperatorChar(char: Char): Boolean = char match {
    case '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '=' | '!' | '@' => true
    case _ => false
  }

  private def isSpecialChar(char: Char): Boolean = char match {
    case '"' | '\'' | '#' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | ';' | '.' | '\\' => true
    case other => isOperatorChar(other)
  }

  private def isWhitespaceOrSpecial(char: Char): Boolean = char.isWhitespace || isSpecialChar(char)

  private def expectNonWhitespace(str: String, start: Int): (Char, Int) = {
    str.indexWhere(!_.isWhitespace, start) match {
      case -1 => ('\u0003', str.length)
      case n  => (str.charAt(n), n)
    }
  }

  private def expectChar(str: String, start: Int): Char = if (str.length <= start)
    parseError(str, start, "Unexpected EOF")
  else
    str.charAt(start)

  private def matchInLine(str: String, offset: Int, stack: List[State], newlineToken: Token): (Token, Int, List[State]) =
    str.charAt(offset) match {
      case '\r' if str.length > offset + 1 && str.charAt(offset + 1) == '\n' => (newlineToken, offset + 2, stack)
      case '\n' => (newlineToken, offset + 1, stack)
      case c if c.isWhitespace =>
        str.indexWhere(_ != c, offset + 1) match {
          case -1 => (EOF, str.length, End :: Nil)
          case n if str.charAt(n) == '\\' && str.length > n + 1 && str.charAt(n + 1) == '\n' => (Whitespace, n + 2, stack)
          case n  => (Whitespace, n, stack)
        }
      case '"' =>
        if (str.startsWith("\"\"\"", offset))
          (Quote("\"\"\""), offset + 3, InTripleString('"') :: stack)
        else
          (Quote("\""), offset + 1, InString('"') :: stack)

      case '\'' =>
        if (str.startsWith("'''", offset))
          (Quote("'''"), offset + 3, InTripleString('\'') :: stack)
        else
          (Quote("'"), offset + 1, InString('\'') :: stack)

      case '#' =>
        str.indexOf('\n', offset + 1) match {
          case -1 => (Comment(str.substring(offset)), str.length, End :: Nil)
          case n  => (Comment(str.substring(offset, n)), n, stack)
        }
      case '(' => (LParen, offset + 1, InImplicitLineJoin(')', RParen) :: stack)
      case '{' => (LBrace, offset + 1, InImplicitLineJoin('}', RBrace) :: stack)
      case '[' => (LBracket, offset + 1, InImplicitLineJoin(']', RBracket) :: stack)
      case ',' => (Comma, offset + 1, stack)
      case '.' if str.length > offset + 1 && str.charAt(offset + 1).isDigit =>
        val (digits, nextOffs) = getDigits(str, offset + 1)
        val (exponentPart, nextOffs1) = if (str.length > nextOffs && str.charAt(nextOffs).toUpper == 'E') {
          val (exponentPart, nextOffs2) = getDigits(str, nextOffs + 1)
          (s"e$exponentPart", nextOffs2)
        } else {
          ("", nextOffs)
        }
        val dec = BigDecimal(s"0.$digits$exponentPart")
        if (str.length > nextOffs1 && str.charAt(nextOffs1).toUpper == 'J') {
          (ImagNum(dec, str.substring(offset, nextOffs1 + 1)), nextOffs1 + 1, stack)
        } else {
          (FloatNum(dec, str.substring(offset, nextOffs1)), nextOffs1, stack)
        }
      case '.' => (Dot, offset + 1, stack)
      case ':' if str.length > offset && str.charAt(offset + 1) == '=' => (ColonEquals, offset + 1, stack)
      case ':' => (Colon, offset + 1, stack)
      case ';' => (Semicolon, offset + 1, stack)
      case '\\' if expectChar(str, offset + 1) == '\n' =>
        (Whitespace, offset + 2, stack)
      case '0' if str.length > offset + 2 =>
        str.charAt(offset + 1) match {
          case 'o' | 'O' =>
            val (digits, nextOffs) = getDigits(str, offset + 2, '7')
            (IntegerNum(BigInt(digits, 8), str.substring(offset, nextOffs)), nextOffs, stack)
          case 'b' | 'B' =>
            val (digits, nextOffs) = getDigits(str, offset + 2, '1')
            (IntegerNum(BigInt(digits, 2), str.substring(offset, nextOffs)), nextOffs, stack)
          case 'x' | 'X' =>
            val (digits, nextOffs) = getDigits(str, offset + 2, 'F')
            (IntegerNum(BigInt(digits, 16), str.substring(offset, nextOffs)), nextOffs, stack)
        }
      case c if c.isDigit =>
        def part(offs: Int, char: Char): (Boolean, Int) = if (offs == -1)
          (false, str.length)
        else if (str.length > offs && str.charAt(offs).toUpper == char) {
          var nextOffs = offs + 1
          while (str.length > nextOffs && str.charAt(nextOffs).isDigit) {
            nextOffs += 1
          }
          (true, nextOffs)
        } else (false, offs)

        val nextOffs = str.indexWhere(!_.isDigit, offset)
        val (fracPart, nextOffs1) = part(nextOffs, '.')
        val (expPart, nextOffs2) = part(nextOffs1, 'E')

        val syntax = str.substring(offset, nextOffs2)

        if (str.length > nextOffs2 && str.charAt(nextOffs2).toLower == 'j') {
          (ImagNum(BigDecimal(syntax), str.substring(offset, nextOffs2 + 1)), nextOffs2 + 1, stack)
        } else {
          if (fracPart || expPart) {
            (FloatNum(BigDecimal(syntax), syntax), nextOffs2, stack)
          } else {
            (IntegerNum(BigInt(syntax), syntax), nextOffs2, stack)
          }
        }

      case c if isOperatorChar(c) =>
        var nextOffs = offset + 1
        while (isOperatorChar(str.charAt(nextOffs))) {
          nextOffs += 1
        }
        (Operator(str.substring(offset, nextOffs)), nextOffs, stack)
      case _ =>
        var nextOffs = offset
        while (str.length > nextOffs && !isWhitespaceOrSpecial(str.charAt(nextOffs))) {
          nextOffs += 1
        }
        (wordOrKeyword(str.substring(offset, nextOffs)), nextOffs, stack)
    }

  private def getDigits(str: String, at: Int, maxDigit: Char = '9'): (String, Int) = {
    val len = str.length
    val allowLetters = maxDigit.isLetter
    var offs = at
    var result = new StringBuilder
    var ch = str.charAt(offs)
    while ((ch.isDigit || (allowLetters && ch.isLetter)) && offs < len) {
      if (ch.toUpper > maxDigit) {
        maxDigit match {
          case '1' => parseError(str, offs, s"Invalid digit '$ch' in binary literal")
          case '7' => parseError(str, offs, s"Invalid digit '$ch' in octal literal")
          case 'F' => parseError(str, offs, "Invalid syntax")
        }
      }
      result.append(ch)
      offs += 1
      if (offs < len)
        ch = str.charAt(offs)
    }
    (result.toString, math.min(str.length, offs))
  }

  private case object Root extends State {
    def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = if (offset < str.length) {
      str.charAt(offset) match {
        case '\r' if str.length > offset + 1 && str.charAt(offset + 1) == '\n' => (Newline, offset + 2, stack)
        case '\n' => (Newline, offset + 1, stack)
        case c if c.isWhitespace =>
          str.indexWhere(!_.isWhitespace, offset) match {
            case -1 => (Indent(str.length - offset), str.length, End :: Nil)
            case n  => (Indent(n - offset), n, InLine :: stack)
          }
        case '#' => InLine.matchNext(str, offset, InLine :: stack)
        case c =>
          (Indent(0), offset, InLine :: stack)
        //InLine.matchNext(str, offset, InLine :: stack)
      }
    } else (EOF, str.length, End :: Nil)
  }

  private case object InLine extends State {
    override def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = str.charAt(offset) match {
      case '\n' if offset > 0 && str.charAt(offset - 1) == '\\' => (Whitespace, offset + 1, stack)
      case '\n' => (Newline, offset + 1, stack.tail)
      case c => matchInLine(str, offset, stack, Newline)
    }
  }

  private case class InString(quoteChar: Char) extends State {
    override def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = str.charAt(offset) match {
      case `quoteChar` => (Quote(quoteChar.toString), offset + 1, stack.tail)
      case other =>
        @tailrec def findClosing(idx: Int): Int = str.indexOf(quoteChar, idx) match {
          case -1 => parseError(str, str.length, s"Expected $quoteChar")
          case next if str.charAt(next - 1) == '\\' => findClosing(next + 1)
          case next => next
        }
        val closingPos = findClosing(offset)
        val content = str.substring(offset, closingPos)
        (Word(content), closingPos, stack)
    }
  }
  
  private case class InTripleString(quoteChar: Char) extends State {
    private val CloseStr = quoteChar.toString * 3
    override def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = str.charAt(offset) match {
      case `quoteChar` if str.startsWith(CloseStr, offset) => (Quote(CloseStr), offset + 3, stack.tail)
      case other =>
        @tailrec def findClosing(idx: Int): Int = str.indexOf(quoteChar, idx) match {
          case -1 => parseError(str, str.length, s"Expected $CloseStr")
          case next if str.charAt(next - 1) == '\\' => findClosing(next + 1)
          case next if str.startsWith(CloseStr, next) => next
          case next => findClosing(next + 1)
        }
        val closingPos = findClosing(offset)
        val content = str.substring(offset, closingPos)
        (Word(content), closingPos, stack)
    }
  }
  
  private case class InImplicitLineJoin(closeChar: Char, closeToken: Token) extends State {
    override def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = {
      str.charAt(offset) match {
        case `closeChar` => (closeToken, offset + 1, stack.tail)
        case other => matchInLine(str, offset, stack, Whitespace)
      }
    }
  }

  private case object End extends State {
    override def matchNext(str: String, offset: Int, stack: List[State]): (Token, Int, List[State]) = (EOF, offset, End :: Nil)
  }

  trait TokenMatcher[T <: Token] {
    def matches(token: Token): Boolean
    def label: String
    def |[T1 >: T <: Token](other: TokenMatcher[T1]): TokenMatcher[T1] = new TokenMatcher[T1] {
      override def matches(token: Token): Boolean = TokenMatcher.this.matches(token) || other.matches(token)
      override val label: String = s"${TokenMatcher.this.label} | ${other.label}"
    }
  }

  abstract class TagTokenMatcher[T <: Token](implicit tag: ClassTag[T]) extends TokenMatcher[T] {
    private val cls = tag.runtimeClass
    override val label: String = cls.getSimpleName
    override def matches(token: Token): Boolean = cls.isInstance(token)
  }
  
  sealed trait Token {
    def value: String
  }

  sealed abstract class AToken[T <: Token](val value: String) extends Token with TokenMatcher[T] { self: T =>
    val label: String = value
    def matches(token: Token): Boolean = token == this
  }
  
  final case class Word(word: String) extends AToken[Word](word)
  object Word extends TagTokenMatcher[Word]

  final case class Keyword(word: String) extends AToken[Keyword](word)
  object Keyword extends TagTokenMatcher[Keyword] {
    val keywords: Set[String] = Set(
      "False",     "await",     "else",      "import",    "pass",
      "None",      "break",     "except",    "in",        "raise",
      "True",      "class",     "finally",   "is",        "return",
      "and",       "continue",  "for",       "lambda",    "try",
      "as",        "def",       "from",      "nonlocal",  "while",
      "assert",    "del",       "global",    "not",       "with",
      "async",     "elif",      "if",        "or",        "yield"
    )
  }

  def wordOrKeyword(str: String): Token = if (Keyword.keywords contains str) Keyword(str) else Word(str)

  sealed abstract class Num[T <: Number](val numericValue: T, syntax: String) extends AToken[Num[T]](syntax) {
    def toLiteral: polynote.python.ast.NumericLiteral[T]
  }
  object Num extends TagTokenMatcher[Num[Number]]

  final case class IntegerNum(intValue: BigInt, syntax: String) extends Num[BigInt](intValue, syntax) {
    override def toLiteral: NumericLiteral[BigInt] = polynote.python.ast.IntegerLiteral(intValue)
  }
  object IntegerNum extends TagTokenMatcher[IntegerNum]

  final case class FloatNum(floatValue: BigDecimal, syntax: String) extends Num[BigDecimal](floatValue, syntax) {
    override def toLiteral: NumericLiteral[BigDecimal] = polynote.python.ast.FloatLiteral(floatValue)
  }
  object FloatNum extends TagTokenMatcher[FloatNum]

  final case class ImagNum(floatValue: BigDecimal, syntax: String) extends Num[BigDecimal](floatValue, syntax) {
    override def toLiteral: NumericLiteral[BigDecimal] = polynote.python.ast.ImaginaryLiteral(floatValue)
  }
  object ImagNum extends TagTokenMatcher[FloatNum]

  final case class Operator(chars: String) extends AToken[Operator](chars)
  object Operator extends TagTokenMatcher[Operator]

  final case class Indent(amount: Int) extends AToken[Indent](" " * amount)
  object Indent extends TagTokenMatcher[Indent]

  sealed abstract class OToken(val value: String) extends Token with TokenMatcher[Token] {
    val label: String = value
    override def matches(token: Token): Boolean = token eq this
  }
  
  final case object Whitespace extends OToken(" ")
  final case object Newline extends OToken("\n")
  final case class Quote(delimiter: String) extends AToken[Quote](delimiter)
  final case class Comment(text: String) extends AToken[Comment](text)
  final case object LParen extends OToken("(")
  final case object RParen extends OToken(")")
  final case object LBracket extends OToken("[")
  final case object RBracket extends OToken("]")
  final case object LBrace extends OToken("{")
  final case object RBrace extends OToken("}")
  final case object Dot extends OToken(".")
  final case object Comma extends OToken(".")
  final case object Semicolon extends OToken(";")
  final case object ColonEquals extends OToken(":=")
  final case object Colon extends OToken(":")
  final case object Empty extends OToken("")
  final case object EOF extends OToken("")


}