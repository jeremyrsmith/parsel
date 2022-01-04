package parsel.ast

import parsel.ast.Util.normalizeFlags

sealed trait Tree extends Product with Serializable {
  def pretty: String
}

sealed trait CompleteTree extends Tree

object Tree {
  def formatBlock(stats: Seq[Statement], prefix: String, indent: String): String = stats.map(_.pretty(prefix + indent, indent)).mkString("\n")
  def formatDecorators(decorators: Seq[Expr], prefix: String): String = if (decorators.isEmpty)
    ""
  else
    decorators.map(e => s"$prefix@${e.pretty}").mkString(s"\n") + "\n"
  def formatElse(orElse: Seq[Statement], prefix: String, indent: String): String = if (orElse.nonEmpty) s"\n${prefix}else:\n" + Tree.formatBlock(orElse, prefix, indent) else ""
}


final case class Module(body: Seq[Statement]) extends CompleteTree {
  def pretty(indent: String): String = body.map(_.pretty("", indent)).mkString("\n")
  override def pretty: String = pretty("    ")

}

sealed trait Statement extends CompleteTree {
  def pretty(prefix: String, indent: String): String
  def pretty(prefix: String): String = pretty(prefix, "    ")
  override def pretty: String = pretty("")

}

final case class FunctionDef(
  name: Name,
  args: Arguments,
  body: Seq[Statement],
  decoratorList: Seq[Expr],
  returns: Option[Expr],
  typeComment: Option[String]
) extends Statement {
  override def pretty(prefix: String, indent: String): String = {
    val annot = returns.fold("")(expr => s" -> ${expr.pretty} ")
    s"${Tree.formatDecorators(decoratorList, prefix)}${prefix}def ${name.pretty}(${args.pretty})$annot:\n${Tree.formatBlock(body, prefix, indent)}"
  }

  def toAsyncFunctionDef: AsyncFunctionDef = AsyncFunctionDef(name, args, body, decoratorList, returns, typeComment)
}

final case class AsyncFunctionDef(
  name: Name,
  args: Arguments,
  body: Seq[Statement],
  decoratorList: Seq[Expr],
  returns: Option[Expr],
  typeComment: Option[String]
) extends Statement {
  override def pretty(prefix: String, indent: String): String = {
    val annot = returns.fold("")(expr => s" -> ${expr.pretty} ")
    s"${Tree.formatDecorators(decoratorList, prefix)}${prefix}async def ${name.pretty}(${args.pretty})$annot:\n${Tree.formatBlock(body, prefix, indent)}"
  }

  def toFunctionDef: FunctionDef = FunctionDef(name, args, body, decoratorList, returns, typeComment)
}

final case class ClassDef(
  name: Name,
  bases: Seq[Expr],
  keywords: Seq[Keyword],
  body: Seq[Statement],
  decoratorList: Seq[Expr]
) extends Statement {
  val basesPretty = Option(bases.map(_.pretty).mkString(", ")).filter(_.nonEmpty)
  val kwsPretty = Option(keywords.map(_.pretty).mkString(", ")).filter(_.nonEmpty)
  override def pretty(prefix: String, indent: String): String =
    s"${Tree.formatDecorators(decoratorList, prefix)}${prefix}class ${name.pretty}(${Seq(basesPretty, kwsPretty).flatten.mkString(", ")}):\n${Tree.formatBlock(body, prefix, indent)}"

}

final case class Return(value: Option[Expr]) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "return" + value.map(" " + _.pretty).getOrElse("")
}

final case class Delete(targets: Seq[Expr]) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "del " + targets.map(_.pretty).mkString(",")
}

final case class Assign(targets: Seq[Expr], value: Expr, typeComment: Option[String]) extends Statement {
  def pretty(prefix: String, indent: String): String = s"$prefix${targets.map(_.pretty).mkString(" = ")} = ${value.pretty}"
}

final case class AugAssign(target: Expr, operator: Operator, value: Expr) extends Statement {
  override def pretty(prefix: String, indent: String): String = s"$prefix${target.pretty} ${operator}= ${value.pretty}"
}

final case class AnnAssign(target: Expr, annotation: Expr, value: Option[Expr], simple: Int) extends Statement {
  override def pretty(prefix: String, indent: String): String = if (simple == 0)
    (Seq(s"$prefix(${target.pretty}): ${annotation.pretty}") ++ value.map(_.pretty)).mkString(" = ")
  else
    (Seq(s"$prefix${target.pretty}: ${annotation.pretty}") ++ value.map(_.pretty)).mkString(" = ")
}

final case class For(target: Expr, iter: Expr, body: Seq[Statement], orElse: Seq[Statement], typeComment: Option[String]) extends Statement {
  override def pretty(prefix: String, indent: String): String = {
    val bodyPretty = Tree.formatBlock(body, prefix, indent)
    s"${prefix}for ${target.pretty} in ${iter.pretty}:\n$bodyPretty${Tree.formatElse(orElse, prefix, indent)}"
  }
}

final case class AsyncFor(target: Expr, iter: Expr, body: Seq[Statement], orElse: Seq[Statement], typeComment: Option[String]) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "async " + For(target, iter, body, orElse, typeComment).pretty(prefix, indent).stripPrefix(prefix)
}

final case class While(test: Expr, body: Seq[Statement], orElse: Seq[Statement]) extends Statement {
  override def pretty(prefix: String, indent: String): String =
    s"${prefix}while ${test.pretty}:\n${Tree.formatBlock(body, prefix, indent)}${Tree.formatElse(orElse, prefix, indent)}"
}

final case class If(test: Expr, body: Seq[Statement], orElse: Seq[Statement]) extends Statement {
  override def pretty(prefix: String, indent: String): String =
    s"${prefix}if ${test.pretty}:\n${Tree.formatBlock(body, prefix, indent)}${Tree.formatElse(orElse, prefix, indent)}"
}

final case class With(items: Seq[WithItem], body: Seq[Statement], typeComment: Option[String]) extends Statement {
  override def pretty(prefix: String, indent: String): String =
    s"${prefix}with (${items.map(_.pretty).mkString(", ")}):\n${Tree.formatBlock(body, prefix, indent)}"
}

final case class AsyncWith(items: Seq[WithItem], body: Seq[Statement], typeComment: Option[String]) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "async " + With(items, body, typeComment).pretty(prefix, indent).stripPrefix(prefix)
}

final case class Raise(exc: Option[Expr], cause: Option[Expr]) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + Seq("raise", Seq(exc, cause).flatMap(_.map(_.pretty)).mkString("from")).mkString(" ")
}

final case class Try(body: Seq[Statement], exceptHandlers: Seq[ExceptHandler], orElse: Seq[Statement], finalBody: Seq[Statement]) extends Statement {
  override def pretty(prefix: String, indent: String): String = {
    val exceptPretty = if (exceptHandlers.nonEmpty) "\n" + exceptHandlers.map(_.pretty(prefix, indent)).mkString("\n") else ""
    val elsePretty = Tree.formatElse(orElse, prefix, indent)
    val finalPretty = if (finalBody.nonEmpty) "\n" + prefix + "finally:" + "\n" + Tree.formatBlock(finalBody, prefix, indent) else ""
    s"${prefix}try:\n${Tree.formatBlock(body, prefix, indent)}$exceptPretty$elsePretty$finalPretty"
  }
}

final case class Assert(test: Expr, msg: Option[Expr]) extends Statement {
  override def pretty(prefix: String, indent: String): String = s"${prefix}assert ${test.pretty}${msg.map(", " + _.pretty).getOrElse("")}"
}

final case class Import(names: Seq[Alias]) extends Statement {
  override def pretty(prefix: String, indent: String): String = s"${prefix}import ${names.map(_.pretty).mkString(", ")}"
}

final case class ImportFrom(module: Option[Name], names: Seq[Alias], level: Option[Int]) extends Statement {
  override def pretty(prefix: String, indent: String): String = {
    val levelStr = level.filterNot(_ == 0).map(i => "." * i)
    prefix + "from " + Seq(levelStr, module.map(_.pretty)).flatten.mkString(" ") + " import " + names.map(_.pretty).mkString(", ")
  }
}

final case class Global(names: Seq[Name]) extends Statement {
  override def pretty(prefix: String, indent: String): String = s"${prefix}global ${names.map(_.pretty).mkString(", ")}"
}

final case class Nonlocal(names: Seq[Name]) extends Statement {
  override def pretty(prefix: String, indent: String): String = s"${prefix}nonlocal ${names.map(_.pretty).mkString(", ")}"
}

final case class ExprStatement(expr: Expr) extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + expr.pretty
}

final case class Pass() extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "pass"
}

final case class Break() extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "break"
}

final case class Continue() extends Statement {
  override def pretty(prefix: String, indent: String): String = prefix + "continue"
}

sealed trait Expr extends CompleteTree {
  private[parsel] final def withExprContext(ctx: ExprContext): Expr = {
    val exprTransformer = new ExprTransformer {
      override def transformExprContext(ctxOld: ExprContext): ExprContext = ctx
    }
    exprTransformer.transformExpr(this)
  }
}

final case class Name(name: String) extends Expr {
  override def pretty: String = name
}

final case class BoolOp(op: BoolOperator, values: Seq[Expr]) extends Expr {
  override def pretty: String = "(" + values.map(_.pretty).mkString(s" ${op.pretty} ") + ")"
}

final case class NamedExpr(target: Expr, value: Expr) extends Expr {
  override def pretty: String = s"(${target.pretty} := ${value.pretty})"
}

final case class BinOp(left: Expr, op: Operator, right: Expr) extends Expr {
  override def pretty: String = s"(${left.pretty} ${op.pretty} ${right.pretty})"
}

final case class UnaryOp(op: UnaryOperator, operand: Expr) extends Expr {
  override def pretty: String = s"(${op.pretty}${operand.pretty})"
}

final case class Lambda(args: Arguments, body: Expr) extends Expr {
  override def pretty: String = s"(lambda ${args.pretty}: ${body.pretty})"
}

final case class IfExp(body: Expr, test: Expr, orElse: Expr) extends Expr {
  override def pretty: String = s"(${body.pretty} if ${test.pretty} else ${orElse.pretty})"
}

final case class Dict(keys: Seq[Expr], values: Seq[Expr]) extends Expr {
  override def pretty: String = {
    val kvs = keys.zip(values).map {
      case (k, v) => s"${k.pretty}: ${v.pretty}"
    }.mkString(", ")

    s"{${kvs}}"
  }
}

final case class ConstructSet(elts: Seq[Expr]) extends Expr {
  override def pretty: String = {
    val values = elts.map(_.pretty).mkString(", ")

    s"{$values}"
  }
}

final case class ListComp(elt: Expr, generators: Seq[Comprehension]) extends Expr {
  override def pretty: String = {
    val gensPretty = generators.map(_.pretty).mkString(" ")
    s"[${elt.pretty} $gensPretty]"
  }
}

final case class SetComp(elt: Expr, generators: Seq[Comprehension]) extends Expr {
  override def pretty: String = {
    val gensPretty = generators.map(_.pretty).mkString(" ")
    s"{${elt.pretty} $gensPretty}"
  }
}

final case class DictComp(key: Expr, value: Expr, generators: Seq[Comprehension]) extends Expr {
  override def pretty: String = {
    val gensPretty = generators.map(_.pretty).mkString(" ")
    s"{${key.pretty}: ${value.pretty} $gensPretty}"
  }
}

final case class GeneratorExp(elt: Expr, generators: Seq[Comprehension]) extends Expr {
  override def pretty: String = {
    val gensPretty = generators.map(_.pretty).mkString(" ")
    s"(${elt.pretty} $gensPretty)"
  }
}

final case class Await(value: Expr) extends Expr {
  override def pretty: String = s"await ${value.pretty}"
}

final case class Yield(value: Option[Expr]) extends Expr {
  override def pretty: String = "(" + (Seq("yield") ++ value.map(_.pretty.toSeq)).mkString(" ") + ")"
}

final case class YieldFrom(value: Expr) extends Expr {
  override def pretty: String = s"(yield from ${value.pretty})"
}

final case class Compare(left: Expr, ops: Seq[ComparisonOperator], comparators: Seq[Expr]) extends Expr {
  override def pretty: String = {
    val opsPretty = ops.zip(comparators).map {
      case (op, expr) => s"${op.pretty} ${expr.pretty}"
    }.mkString(" ")
    s"(${left.pretty} $opsPretty)"
  }
}

final case class Call(func: Expr, args: Seq[Expr], keywords: Seq[Keyword]) extends Expr {
  override def pretty: String = {
    val kwsPretty = keywords.map(_.pretty)
    val argsPretty = (args.map(_.pretty) ++ kwsPretty).mkString(", ")
    s"${func.pretty}($argsPretty)"
  }
}

final case class FormattedValue(value: Expr, conversion: Option[Int], formatSpec: Option[JoinedStr]) extends Expr {
  override def pretty: String = {
    val conversionPretty = conversion.filterNot(_ <= 0).filter(_ < 128).map(i => "!" + Character.toString(i.toChar)).getOrElse("")
    val formatPretty = formatSpec.map {
      case JoinedStr(values) => ":" + values.map {
        case Constant(literal) =>
          literal match {
            case s: StringLiteral => s.escaped
            case b: BytesLiteral  => b.escaped
            case l => l.pretty
          }
        case expr => expr.pretty
      }.mkString
    }.getOrElse("")
    s"{${value.pretty}$conversionPretty$formatPretty}"
  }

  override def equals(that: Any): Boolean = that match {
    case FormattedValue(`value`, conversion, `formatSpec`) =>
      (conversion == this.conversion || conversion.isEmpty && this.conversion.contains(-1) || conversion.contains(-1) && this.conversion.isEmpty)
    case _ => false
  }
}

final case class JoinedStr(values: Seq[Expr]) extends Expr {
  override def pretty: String = "f'" + values.map {
    case Constant(literal) =>
      literal match {
        case s: StringLiteral => s.escaped
        case b: BytesLiteral  => b.escaped
        case l => l.pretty
      }
    case expr => expr.pretty
  }.mkString + "'"
}

final case class Constant[A](value: Literal[A]) extends Expr {
  override def pretty: String = value.pretty
}

final case class Attribute(value: Expr, attr: Name, ctx: ExprContext) extends Expr {
  override def pretty: String = s"${value.pretty}.${attr.pretty}"
}

final case class Subscript(value: Expr, slice: Expr, ctx: ExprContext) extends Expr {
  override def pretty: String = {
    val slicePretty = slice match {
      case ConstructTuple(elts, _) => elts.map(_.pretty).mkString(", ")
      case expr => expr.pretty
    }
    s"${value.pretty}[$slicePretty]"
  }
}

final case class Starred(value: Expr, ctx: ExprContext) extends Expr {
  override def pretty: String = s"*${value.pretty}"
}

final case class ConstructList(elts: Seq[Expr], ctx: ExprContext) extends Expr {
  override def pretty: String = s"[${elts.map(_.pretty).mkString(", ")}]"
}

final case class ConstructTuple(elts: Seq[Expr], ctx: ExprContext) extends Expr {
  override def pretty: String = s"(${elts.map(_.pretty).mkString(", ")})"
}

final case class Slice(lower: Option[Expr], upper: Option[Expr], step: Option[Expr]) extends Expr {
  override def pretty: String = (lower.map(_.pretty), upper.map(_.pretty), step.map(_.pretty)) match {
    case (Some(l), Some(u), Some(s)) => s"$l:$u:$s"
    case (Some(l), Some(u), None) => s"$l:$u"
    case (Some(l), None, Some(s)) => s"$l::$s"
    case (Some(l), None, None) => l
    case (None, Some(u), maybeS) => s":$u" + maybeS.map(":" + _).getOrElse("")
    case (None, None, maybeS) => "::" + maybeS.getOrElse("")
  }
}

sealed trait Literal[+A] {
  def value: A
  def pretty: String
}

final case class StringLiteral(value: String, flags: Option[String] = None) extends Literal[String] {
  def escaped: String = value.flatMap {
    case c@('\\' | '\'' | '"') => s"\\$c"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case 7 => "\\a"
    case 8 => "\\b"
    case 11 => "\\v"
    case 12 => "\\f"
    case c if Character.isISOControl(c) => c.toInt.formatted("%#04x")
    case c => Seq(c)
  }

  override def pretty: String = {
    "'" + escaped + "'"
  }

  override def equals(obj: Any): Boolean = obj match {
    case StringLiteral(value, _) => value == this.value // flags are erased during parsing
    case _ => false
  }
}

final case class BytesLiteral(value: String, flags: String) extends Literal[String] {
  def escaped: String = value.flatMap {
    case c@('\\' | '\'' | '"') => s"\\$c"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case 7 => "\\a"
    case 8 => "\\b"
    case 11 => "\\v"
    case 12 => "\\f"
    case c => Seq(c)
  }.replace("0x", "0\\x")
  override def pretty: String = s"b'${escaped}'"
  override def equals(obj: Any): Boolean = obj match {
    case BytesLiteral(value, _) => value == this.value
    case _ => false
  }
}

final case class BooleanLiteral(value: Boolean) extends Literal[Boolean] {
  override def pretty: String = if (value) "True" else "False"
}

case object NoneLiteral extends Literal[None.type] {
  val value: None.type = None
  override def pretty: String = "None"
}

sealed abstract class NumericLiteral[A <: Number : Numeric] extends Literal[A] {
  def numeric: Numeric[A] = implicitly
  override def pretty: String = value.toString
}

final case class IntegerLiteral(value: BigInt) extends NumericLiteral[BigInt] {
  override def equals(that: Any): Boolean = that match {
    case IntegerLiteral(thatValue) => value == thatValue
    case FloatLiteral(thatValue)   => value == thatValue
    case _ => false
  }

  override def pretty: String = value.toString
}

final case class FloatLiteral(value: BigDecimal) extends NumericLiteral[BigDecimal] {
  override def equals(that: Any): Boolean = that match {
    case IntegerLiteral(thatValue) => value == thatValue
    case FloatLiteral(thatValue)   => value == thatValue
    case _ => false
  }

  override def pretty: String = value.scale match {
    case 0 => value.setScale(1).toString()
    case _ => value.toString()
  }
}

final case class ImaginaryLiteral(value: BigDecimal) extends NumericLiteral[BigDecimal] {
  override def pretty: String = value.toString() + "j"
}

case object Ellipsis extends Literal[String] {
  val value: String = "..."
  override def pretty: String = "..."
}

sealed trait ExprContext
case object Load extends ExprContext
case object Store extends ExprContext
case object Del extends ExprContext

object ExprContext {
  implicit val load: ExprContext = Load
  implicit val store: ExprContext = Store
  implicit val del: ExprContext = Del
}

sealed trait BoolOperator {
  def pretty: String = this match {
    case And => "and"
    case Or  => "or"
  }
}
case object And extends BoolOperator
case object Or extends BoolOperator

sealed abstract class Operator(val pretty: String) {
  def mk(l: Expr, r: Expr): Expr = BinOp(l, this, r)
  override def toString: String = pretty
}

case object Add      extends Operator("+")
case object Sub      extends Operator("-")
case object Mult     extends Operator("*")
case object MatMult  extends Operator("@")
case object Div      extends Operator("/")
case object Mod      extends Operator("%")
case object Pow      extends Operator("**")
case object LShift   extends Operator("<<")
case object RShift   extends Operator(">>")
case object BitOr    extends Operator("|")
case object BitXor   extends Operator("^")
case object BitAnd   extends Operator("&")
case object FloorDiv extends Operator("//")

sealed abstract class UnaryOperator(val pretty: String)
case object Invert extends UnaryOperator("~")
case object Not extends UnaryOperator("not ")
case object UAdd extends UnaryOperator("+")
case object USub extends UnaryOperator("-")

sealed abstract class ComparisonOperator(val pretty: String)
case object Eq extends ComparisonOperator("==")
case object NotEq extends ComparisonOperator("!=")
case object Lt extends ComparisonOperator("<")
case object LtE extends ComparisonOperator("<=")
case object Gt extends ComparisonOperator(">")
case object GtE extends ComparisonOperator(">=")
case object Is extends ComparisonOperator("is")
case object IsNot extends ComparisonOperator("is not")
case object In extends ComparisonOperator("in")
case object NotIn extends ComparisonOperator("not in")

final case class Comprehension(target: Expr, iter: Expr, ifs: Seq[Expr], isAsync: Int) extends Tree {
  override def pretty: String = {
    val asyncPretty = if (isAsync > 0) "async " else ""
    val ifsPretty = if (ifs.nonEmpty) " " + ifs.map(e => s"if ${e.pretty}").mkString(" ") else ""
    s"${asyncPretty} for ${target.pretty} in ${iter.pretty}$ifsPretty"
  }
}

final case class ExceptHandler(typ: Option[Expr], name: Option[Name], body: Seq[Statement]) extends Tree {
  override def pretty: String = pretty("", "    ")
  def pretty(prefix: String, indent: String): String = {
    val typPretty = typ.map(e => s" ${e.pretty}").getOrElse("")
    val namePretty = name.map(n => s" as ${n.pretty}").getOrElse("")
    val bodyPretty = Tree.formatBlock(body, prefix, indent)
    s"${prefix}except${typPretty}${namePretty}:\n$bodyPretty"
  }
}

final case class Arguments(
  posOnlyArgs: Seq[Arg],
  args: Seq[Arg],
  varArg: Option[Arg],
  kwOnlyArgs: Seq[Arg],
  kwDefaults: Seq[Option[Expr]],
  kwArg: Option[Arg],
  defaults: Seq[Expr]
) extends Tree {
  override def pretty: String = {
    def formatArgsDefaults(args: Seq[Arg], defaults: Seq[Expr]): String =
      formatArgsPaddedDefaults(
        args,
        defaults.map(e => Some(e)) match {
          case ds if ds.size < args.size => Seq.fill(args.size - ds.size)(None) ++ ds
          case ds => ds.take(args.size)
        }
      )

    def formatArgsPaddedDefaults(args: Seq[Arg], paddedDefaults: Seq[Option[Expr]]): String =
      args.zip(paddedDefaults).map {
        case (arg, Some(dv)) => s"${arg.pretty} = ${dv.pretty}"
        case (arg, None) => arg.pretty
      }.mkString(", ")

    val posOnlyPretty = Some(posOnlyArgs).filter(_.nonEmpty).map {
      posOnlyArgs => formatArgsDefaults(posOnlyArgs, defaults.dropRight(args.size))
    }

    val argsPretty = Some(args).filter(_.nonEmpty).map {
      args => formatArgsDefaults(args, defaults.takeRight(args.size))
    }

    val posOnlySep = posOnlyPretty.map(_ => "/")

    val varargPretty = varArg.map(arg => s"*${arg.pretty}")

    val kwsPretty = Option(kwOnlyArgs).filter(_.nonEmpty).map {
      kwOnlyArgs => formatArgsPaddedDefaults(kwOnlyArgs, kwDefaults)
    }

    val kwArgPretty = kwArg.map(arg => s"**${arg.pretty}")

    val kwSep = (varargPretty, kwsPretty) match {
      case (s@Some(_), _) => s
      case (None, Some(_)) => Some("*")
      case (None, None) => None
    }

    val strs = Seq(posOnlyPretty, posOnlySep, argsPretty, kwSep, kwsPretty, kwArgPretty).flatten
    strs.mkString(",")
  }

}

object Arguments {
  import Util._
  val empty: Arguments = Arguments(Nil, Nil, None, Nil, Nil, None, Nil)

  def fromParams(params: Params): Arguments = Arguments(
    posOnlyArgs = params.posOnlyParams.toArgs,
    args = params.params.toArgs,
    varArg = params.varArg.map(_.arg),
    kwOnlyArgs = params.kwOnlyParams.toArgs,
    kwDefaults = params.kwOnlyParams.paddedDefaults,
    kwArg = params.kwParam.map(_.arg),
    defaults = params.posOnlyParams.defaults ++ params.params.defaults
  )

  implicit class OptionOps(val self: Option[Arguments]) extends AnyVal {
    def orEmpty: Arguments = self.getOrElse(empty)
  }
}

final case class Arg(arg: Name, annotation: Option[Expr], typeComment: Option[String]) extends Tree {
  override def pretty: String = {
    val annotPretty = annotation.map(e => s": ${e.pretty}").getOrElse("")
    s"${arg.pretty}$annotPretty"
  }
}
object Arg {
  def name(name: Name): Arg = Arg(name, None, None)
}

final case class Keyword(arg: Option[Name], value: Expr) extends Tree {
  override def pretty: String = arg match {
    case Some(name) => s"${name.pretty} = ${value.pretty}"
    case None => s"**${value.pretty}"
  }
}

final case class Alias(name: Name, asName: Option[Name]) extends Tree {
  override def pretty: String = Seq(Some(name), asName).flatten.map(_.pretty).mkString(" as ")

}

final case class WithItem(contentExpr: Expr, optionalVars: Option[Expr]) extends Tree {
  override def pretty: String = Seq(Some(contentExpr), optionalVars).flatten.map(_.pretty).mkString(" as ")

}

sealed trait Position {
  def offset: Int
  def start: Int
  def end: Int
}

final case class RangePosition(start: Int, end: Int) extends Position {
  def offset: Int = start
}