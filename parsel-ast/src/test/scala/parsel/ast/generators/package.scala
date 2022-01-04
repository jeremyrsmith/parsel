package parsel.ast

import org.scalacheck.{Arbitrary, Gen, Shrink}
import Arbitrary.arbitrary
import parsel.ast.Util.{Param, Params}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

package object generators {

  import org.scalacheck.ScalacheckShapeless._

  // all numbers are positive in the AST – a negative number is USub(absNumber)
  private implicit val arbitraryBigDecimal: Arbitrary[BigDecimal] = Arbitrary(Arbitrary.arbBigDecimal.arbitrary.map(_.abs))
  private implicit val arbitraryBigInt: Arbitrary[BigInt] = Arbitrary(Arbitrary.arbBigInt.arbitrary.map(_.abs))

  // strings have to be valid UTF-8 characters
  private val utf8Encoder = StandardCharsets.UTF_8.newEncoder()
  private def isValidUTF8(char: Char): Boolean = utf8Encoder.canEncode(char)
  private implicit val arbString: Arbitrary[String] = Arbitrary(Gen.listOf(Arbitrary.arbChar.arbitrary.suchThat(isValidUTF8)).map(_.mkString))

  private val traceTimes = new mutable.HashMap[String, Long]()

  private def traced[A](label: String)(gen: Gen[A]): Gen[A] = Gen.const("").flatMap {
    _ =>
      val startTime = System.currentTimeMillis()
      gen.map {
        result =>
          val time = System.currentTimeMillis() - startTime
          traceTimes.put(label, traceTimes.getOrElse(label, 0L) + time)
          result
      }
  }

  private val keywords = Set(
    "False",     "await",     "else",      "import",    "pass",
    "None",      "break",     "except",    "in",        "raise",
    "True",      "class",     "finally",   "is",        "return",
    "and",       "continue",  "for",       "lambda",    "try",
    "as",        "def",       "from",      "nonlocal",  "while",
    "assert",    "del",       "global",    "not",       "with",
    "async",     "elif",      "if",        "or",        "yield"
  )

  val genName: Gen[Name] = {
    for {
      start <- Gen.oneOf(Gen.const('_'), Gen.alphaChar)
      rest  <- Gen.listOf(Gen.oneOf(Gen.const('_'), Gen.alphaNumChar))
    } yield Name((start :: rest).mkString)
  }.suchThat(name => !keywords.contains(name.name))

  val genBoolOperator: Gen[BoolOperator] = Gen.oneOf(And, Or)

  lazy val genBoolOp: Gen[BoolOp] = for {
    op <- genBoolOperator
    a  <- genExprInner
    as <- Gen.nonEmptyListOf(genExprInner)
  } yield BoolOp(op, a :: as)

  lazy val genNamedExpr: Gen[NamedExpr] = for {
    target <- genExprInner
    value  <- genExprInner
  } yield NamedExpr(target, value)

  val genOperator: Gen[Operator] = Gen.oneOf(Add, Sub, Mult, MatMult, Div, Mod, Pow, LShift, RShift, BitOr, BitXor, BitAnd, FloorDiv)

  lazy val genBinOp: Gen[BinOp] = for {
    lhs <- genExprInner
    op  <- genOperator
    rhs <- genExprInner
  } yield BinOp(lhs, op, rhs)

  val genUnaryOperator: Gen[UnaryOperator] = Gen.oneOf(Invert, Not, UAdd, USub)

  lazy val genUnaryOp: Gen[UnaryOp] = for {
    op      <- genUnaryOperator
    operand <- genExprInner
  } yield UnaryOp(op, operand)

  lazy val genTypeExpr: Gen[Expr] = for {
    // TODO: subscripting for generics, maybe?
    first <- Gen.alphaChar
    rest  <- Gen.alphaNumStr
  } yield Name(first.toUpper.toString + rest)

  lazy val genParamNoAnnot: Gen[Param] = for {
    name    <- genName
    default <- Gen.option(genExprInner)
  } yield Param(name, None, default)

  lazy val genParam: Gen[Param] = for {
    param <- genParamNoAnnot
    annot <- Gen.option(genTypeExpr)
  } yield param.copy(typ = annot)

  def genParams(genParam: Gen[Param]): Gen[Params] = for {
    posOnly <- Gen.listOf(genParam)
    params  <- Gen.listOf(genParam)
    varArg  <- Gen.option(genParam)
    kwOnly  <- Gen.listOf(genParam.suchThat(_.default.nonEmpty))
    kwParam <- Gen.option(genParam.map(p => p.copy(default = None)))
  } yield {
    val (posOnlyWithDefault, posOnlyNoDefault) = posOnly.partition(_.default.nonEmpty)
    val (paramsWithDefault, paramsNoDefault) = params.partition(_.default.nonEmpty)
    Params(
      posOnlyNoDefault ++ posOnlyWithDefault,
      if (posOnlyWithDefault.isEmpty) paramsNoDefault ++ paramsWithDefault else paramsWithDefault,
      varArg,
      kwOnly,
      kwParam
    )
  }

  lazy val genArguments: Gen[Arguments] = genParams(genParam).map(Arguments.fromParams)
  lazy val genLambdaArguments: Gen[Arguments] = genParams(genParamNoAnnot).map(Arguments.fromParams)

  lazy val genLambda: Gen[Lambda] = for {
    args <- genLambdaArguments
    body <- genExprInner
  } yield Lambda(args, body)

  lazy val genIfExp: Gen[IfExp] = for {
    body <- genExprInner
    test <- genExprInner
    orElse <- genExprInner
  } yield IfExp(body, test, orElse)

  lazy val genDict: Gen[Dict] = for {
    keys   <- Gen.listOf(genExprInner)
    values <- Gen.listOfN(keys.size, genExprInner)
  } yield Dict(keys, values)

  lazy val genSet: Gen[ConstructSet] = for {
    values <- Gen.nonEmptyListOf(genExprInner)
  } yield ConstructSet(values)

  lazy val genTarget: Gen[Expr] = Gen.oneOf(
    genName,
    Gen.nonEmptyListOf(genName).map {
      case one :: Nil => one
      case many => ConstructTuple(many, Store)
    }
  )

  lazy val genComprehension: Gen[Comprehension] = for {
    target <- genTarget
    iter   <- genExprInner
    ifs    <- Gen.listOf(genExprInner)
    isAsync <- Gen.oneOf(0, 1)
  } yield Comprehension(target, iter, ifs, isAsync)

  lazy val genListComp: Gen[ListComp] = for {
    elt        <- genExprInner
    generators <- Gen.nonEmptyListOf(genComprehension)
  } yield ListComp(elt, generators)

  lazy val genSetComp: Gen[SetComp] = for {
    elt        <- genExprInner
    generators <- Gen.nonEmptyListOf(genComprehension)
  } yield SetComp(elt, generators)

  lazy val genDictComp: Gen[DictComp] = for {
    key        <- genExprInner
    value      <- genExprInner
    generators <- Gen.nonEmptyListOf(genComprehension)
  } yield DictComp(key, value, generators)

  lazy val genGeneratorExp: Gen[GeneratorExp] = for {
    elt        <- genExprInner
    generators <- Gen.nonEmptyListOf(genComprehension)
  } yield GeneratorExp(elt, generators)

  lazy val genAwait: Gen[Await] = genExprInner.map(Await)

  lazy val genYield: Gen[Yield] = Gen.option(genExprInner).map(Yield)

  lazy val genYieldFrom: Gen[YieldFrom] = genExprInner.map(YieldFrom)

  val genComparisonOperator: Gen[ComparisonOperator] = Gen.oneOf(Eq, NotEq, Lt, LtE, Gt, GtE, Is, IsNot, In, NotIn)

  lazy val genCompare: Gen[Compare] = for {
    lhs         <- genExprInner
    comparators <- Gen.nonEmptyListOf(genExprInner)
    compareOps  <- Gen.listOfN(comparators.size, genComparisonOperator)
  } yield Compare(lhs, compareOps, comparators)

  lazy val genNamedArg: Gen[Keyword] = for {
    name <- genName
    expr <- genExprInner
  } yield Keyword(Some(name), expr)

  lazy val genKwArgs: Gen[List[Keyword]] = for {
    namedArgs <- Gen.listOf(genNamedArg)
    kwArg     <- Gen.option(genExprInner.map(expr => Keyword(None, expr)))
  } yield namedArgs ++ kwArg.toList

  lazy val genCall: Gen[Call] = for {
    func     <- genExprInner
    args     <- Gen.listOf(genExprInner)
    keywords <- genKwArgs
  } yield Call(func, args, keywords)

  val genStringLiteral: Gen[StringLiteral] = for {
    content <- arbitrary[String]
    flags   <- Gen.option(Gen.oneOf("r", "u", "R", "U"))
  } yield StringLiteral(content, flags)

  val genBytesLiteral: Gen[BytesLiteral] = for {
    content <- Gen.asciiStr
    flags   <- Gen.oneOf("b", "B", "br", "Br", "bR", "BR", "rb", "rB", "Rb", "RB")
  } yield BytesLiteral(content, flags)

  lazy val genConstant: Gen[Constant[_]] = Gen.oneOf(
    genStringLiteral,
    genBytesLiteral,
    arbitrary[BigInt].map(IntegerLiteral),
    arbitrary[BigDecimal].map(FloatLiteral),
    arbitrary[BigDecimal].map(ImaginaryLiteral)
  ).map(lit => Constant(lit))

  private def normalizeJoinedStrParts(exprs: List[Expr]): List[Expr] = exprs.foldRight(List.empty[Expr]) {
    case (Constant(StringLiteral("", _)), accum) => accum
    case (Constant(StringLiteral(str2, _)), Constant(StringLiteral(str1, _)) :: rest) => Constant(StringLiteral(str1 + str2)) :: rest
    case (next, accum) => next :: accum
  }

  lazy val genJoinedStr: Gen[JoinedStr] = Gen.lzy(Gen.listOf(Gen.oneOf(
    genFormattedValue,
    genStringLiteral.map(lit => Constant(lit))
  ))).map(normalizeJoinedStrParts).map(JoinedStr)

  lazy val genFormatSpec: Gen[JoinedStr] = {
    def opt(gen: Gen[String]): Gen[String] = Gen.oneOf(gen, Gen.const(""))
    for {
      fill <- opt(Gen.asciiPrintableChar.map(_.toString))
      align <- opt(Gen.oneOf("<", ">", "=", "^"))
      alt   <- Gen.oneOf("#", "")
      pad   <- Gen.oneOf("0", "")
      width <- Gen.option(Gen.oneOf(arbitrary[BigInt].map(i => Constant(StringLiteral(i.toString))), genBareFormattedValue))
      groupOpt <- Gen.option(Gen.oneOf(",", "_").map(str => Constant(StringLiteral(str))))
      precision <- Gen.option(Gen.oneOf(arbitrary[BigInt].map(i => Constant(StringLiteral(i.toString))), genBareFormattedValue))
      typ <- Gen.option(Gen.oneOf("s", "b", "d", "o", "x", "X", "n", "e", "E", "f", "F", "g", "G", "n", "%").map(str => Constant(StringLiteral(str))))
    } yield JoinedStr(
      normalizeJoinedStrParts(List(
        Option(s"$fill$align$alt$pad").filter(_.nonEmpty).map(str => Constant(StringLiteral(str))),
        width,
        groupOpt,
        precision,
        typ
      ).flatMap(_.toList))
    )
  }

  private val formattedValueCleaner = new ExprTransformer {
    override def transformExpr(expr: Expr): Expr = expr match {
      case Constant(StringLiteral(_, _)) | JoinedStr(_) => Name("no_strings")
      case other =>
        super.transformExpr(other)
    }
  }

  lazy val genFormattedValue: Gen[FormattedValue] = for {
    value      <- genName //genExprInner – restricting to {name} for now; otherwise it gets too crazy
    conversion <- Gen.option(Gen.oneOf(-1, 115, 114, 97))
    formatSpec <- Gen.option(genFormatSpec)
  } yield FormattedValue(value, conversion, formatSpec)

  lazy val genBareFormattedValue: Gen[FormattedValue] = genExprInner.map(formattedValueCleaner.transformExpr).map(FormattedValue(_, None, None))

  lazy val genAttribute: Gen[Attribute] = for {
    value <- genExprInner.suchThat {
      case Constant(IntegerLiteral(_)) => false
      case _ => true
    }
    name  <- genName
  } yield Attribute(value, name, Load)

  lazy val genSlice: Gen[Slice] = for {
    lower <- Gen.option(genExprInner)
    upper <- Gen.option(genExprInner)
    step  <- Gen.option(genExprInner)
  } yield Slice(lower, upper, step)

  lazy val genSlices: Gen[Expr] = for {
    first <- genSlice
    rest  <- Gen.option(Gen.listOf(genSlice)).map(_.toList.flatten)
  } yield rest match {
    case Nil  => first
    case rest => ConstructTuple(first :: rest, Load)
  }

  lazy val genSubscript: Gen[Subscript] = for {
    value <- genExprInner
    slice <- genSlices
  } yield Subscript(value, slice, Load)

  // Starred can't be generated directly.

  lazy val genList: Gen[ConstructList] = Gen.listOf(genExprInner).map(ConstructList(_, Load))

  lazy val genTuple: Gen[ConstructTuple] = for {
    first  <- genExprInner
    second <- genExprInner
    rest   <- Gen.listOf(genExprInner)
  } yield ConstructTuple(first :: second :: rest, Load)

  lazy val genExprInner: Gen[Expr] = Gen.size.flatMap {
    case 0 | 1 => Gen.oneOf(genName, genConstant)
    case _     => Gen.sized(s => Gen.resize(s / 16, genExpr))
  }

  lazy val genGroup: Gen[Expr] = Gen.oneOf(genYield, genYieldFrom, genNamedExpr)

  lazy val genAtom: Gen[Expr] = Gen.oneOf(
    genName,
    genConstant,
    Gen.oneOf(genTuple, genGroup, genGeneratorExp),
    Gen.oneOf(genList, genListComp),
    Gen.oneOf(genDict, genSet, genDictComp, genSetComp)
  )

  lazy val genExpr: Gen[Expr] = Gen.oneOf(
    genAtom,
    genBoolOp,
    genBinOp,
    genUnaryOp,
    genLambda,
    genIfExp,
    genDict,
    genSet,
    genList,
    genTuple,
    genSubscript,
    genAttribute,
    genConstant,
    //genJoinedStr,
    genListComp,
    genSetComp,
    genDictComp,
    genGeneratorExp,
    genCompare,
    genCall
  )

  implicit val arbExpr: Arbitrary[Expr] = Arbitrary {
    Gen.const("").flatMap {
      _ =>
        val startTime = System.currentTimeMillis()
        genExpr.map {
          expr =>
            val time = System.currentTimeMillis() - startTime
            //println(s"Generated expr of length ${expr.pretty.length} in $time ms")
            expr
        }
    }
  }

//  private val shrinkTransformer = new TreeTransformer {
//    override def transformExprs(exprs: Seq[Expr]): Seq[Expr] = exprs.size match {
//      case 1 => exprs
//      case n => exprs.permutations.find(_ => scala.util.Random.nextDouble() > 0.5).getOrElse(exprs.take(n / 2))
//    }
//
//    private def tryShrinkExpr(expr: Expr): Expr = nextExprs(expr).headOption.getOrElse(expr)
//    private def tryShrinkExprs(exprs: Seq[Expr]): Seq[Expr] = exprs.map(tryShrinkExpr)
//
//    override def transformComprehension(comprehension: Comprehension): Comprehension = comprehension match {
//      case Comprehension(target, iter, ifs, isAsync) => Comprehension(
//        tryShrinkExpr(target),
//        tryShrinkExpr(iter),
//        tryShrinkExprs(ifs),
//        isAsync
//      )
//    }
//  }

  private def nextExprs(expr: Expr): Stream[Expr] = expr match {
    case Name(_) | Constant(_) => Stream.empty
    case NamedExpr(target, value) =>
      val children = Stream(target, value)
      children #::: children.flatMap(nextExprs)
    case ConstructTuple(elts, _) => elts.toStream #::: elts.toStream.flatMap(nextExprs)
    case ConstructList(elts, _) => elts.toStream #::: elts.toStream.flatMap(nextExprs)
    case Dict(keys, values) => Stream(keys.toStream, values.toStream, keys.toStream.flatMap(nextExprs), values.toStream.flatMap(nextExprs)).flatten
    case ConstructSet(elts) => elts.toStream #::: elts.toStream.flatMap(nextExprs)
    case BoolOp(op, values) => values.toStream
    case ListComp(elt, generators) =>
      val parts = elt #:: generators.toStream.flatMap(comp => Stream(comp.target, comp.iter) ++ comp.ifs.toStream)
      parts #::: parts.flatMap(nextExprs)
    case SetComp(elt, generators) =>
      val parts = elt #:: generators.toStream.flatMap(comp => Stream(comp.target, comp.iter) ++ comp.ifs.toStream)
      parts #::: parts.flatMap(nextExprs)
    case DictComp(key, value, generators) =>
      val parts = key #:: value #:: generators.toStream.flatMap(comp => Stream(comp.target, comp.iter) ++ comp.ifs.toStream)
      parts #::: parts.flatMap(nextExprs)
    case GeneratorExp(elt, generators) =>
      val parts = elt #:: generators.toStream.flatMap(comp => Stream(comp.target, comp.iter) ++ comp.ifs.toStream)
      parts #::: parts.flatMap(nextExprs)
    case Lambda(_, body) => body #:: nextExprs(body)
    case Await(value) => value #:: nextExprs(value)
    case Yield(Some(value)) => value #:: nextExprs(value)
    case Yield(None) => Stream.empty
    case YieldFrom(value) => value #:: nextExprs(value)
    case Compare(left, _, comparators) =>
      val parts = left #:: comparators.toStream
      parts #::: parts.flatMap(nextExprs)
    case Call(func, args, keywords) =>
      val parts = func #:: args.toStream #::: keywords.toStream.map(_.value)
      parts #::: parts.flatMap(nextExprs)
    case FormattedValue(value, _, formatSpec) =>
      val parts = value #:: formatSpec.toStream.flatMap(_.values.toStream)
      parts #::: Stream(FormattedValue(Name("a"), None, formatSpec)) #::: parts.flatMap(nextExprs)
    case JoinedStr(values) =>
      val parts = values.toStream.flatMap {
        case FormattedValue(value, _, formatSpec) =>
          value #:: (formatSpec: Option[Expr]).toStream
        case expr => Stream(expr)
      }
      parts #::: parts.flatMap(nextExprs)
    case Attribute(value, _, _) => value #:: nextExprs(value)
    case Subscript(value, slice, _) =>
      val sliceParts = slice match {
        case ConstructTuple(elts, _) => elts.toStream.flatMap(nextExprs)
        case other => nextExprs(other)
      }
      val parts = value #:: sliceParts
      parts #::: parts.flatMap(nextExprs)
    case Slice(lower, upper, step) =>
      val parts = lower.toStream #::: upper.toStream #::: step.toStream
      parts #::: parts.flatMap(nextExprs)
    case BinOp(lhs, _, rhs) =>
      val parts = Stream(lhs, rhs)
      parts #::: parts.flatMap(nextExprs)
    case IfExp(body, test, orElse) =>
      val parts = Stream(body, test, orElse)
      parts #::: parts.flatMap(nextExprs)
    case Starred(value, _) => value #:: nextExprs(value)
    case UnaryOp(_, operand) => operand #:: nextExprs(operand)
  }

  implicit val shrinkExpr: Shrink[Expr] = Shrink { expr =>
    //println(s"Shrinking ${expr.pretty}")
    nextExprs(expr)
  }

}
