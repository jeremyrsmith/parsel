package polynote.python.ast

sealed trait Tree extends Product with Serializable

final case class Ident(name: String) extends Expr

sealed trait Mod extends Tree
final case class Module(body: Seq[Statement]) extends Mod
final case class Interactive(body: Seq[Statement]) extends Mod
final case class Expression(body: Expr) extends Mod
final case class FunctionType(argTypes: Seq[Expr], returns: Expr) extends Mod

sealed trait Statement extends Tree

trait FunctionDefLike extends Statement {
  def withDecorators(decorators: Seq[Expr]): FunctionDefLike
}

final case class FunctionDef(
  name: Ident,
  args: Arguments,
  body: Seq[Statement],
  decoratorList: Seq[Expr],
  returns: Option[Expr],
  typeComment: Option[String]
) extends FunctionDefLike {
  override def withDecorators(decorators: Seq[Expr]): FunctionDefLike = copy(decoratorList = decorators)
}

final case class AsyncFunctionDef(
  name: Ident,
  args: Arguments,
  body: Seq[Statement],
  decoratorList: Seq[Expr],
  returns: Option[Expr],
  typeComment: Option[String]
) extends FunctionDefLike {
  override def withDecorators(decorators: Seq[Expr]): FunctionDefLike = copy(decoratorList = decorators)
}

final case class ClassDef(
  name: Ident,
  bases: Seq[Expr],
  keywords: Seq[Keyword],
  body: Seq[Statement],
  decoratorList: Seq[Expr]
) extends Statement

final case class Return(value: Option[Expr]) extends Statement
final case class Delete(targets: Seq[Expr]) extends Statement
final case class Assign(targets: Seq[Expr], value: Expr, typeComment: Option[String]) extends Statement
final case class AugAssign(target: Expr, operator: Operator, value: Expr) extends Statement
final case class AnnAssign(target: Expr, annotation: Expr, value: Option[Expr], simple: Int) extends Statement
final case class For(target: Expr, iter: Expr, body: Seq[Statement], orElse: Seq[Statement], typeComment: Option[String]) extends Statement
final case class AsyncFor(target: Expr, iter: Expr, body: Seq[Statement], orElse: Seq[Statement], typeComment: Option[String]) extends Statement
final case class While(test: Expr, body: Seq[Statement], orElse: Seq[Statement]) extends Statement
final case class If(test: Expr, body: Seq[Statement], orElse: Seq[Statement]) extends Statement
final case class With(items: Seq[WithItem], body: Seq[Statement], typeComment: Option[String]) extends Statement
final case class AsyncWith(items: Seq[WithItem], body: Seq[Statement], typeComment: Option[String]) extends Statement
final case class Raise(exc: Option[Expr], cause: Option[Expr]) extends Statement
final case class Try(body: Seq[Statement], exceptHandlers: Seq[ExceptHandler], orElse: Seq[Statement], finalBody: Seq[Statement]) extends Statement
final case class Assert(test: Expr, msg: Option[Expr]) extends Statement
final case class Import(names: Seq[Alias]) extends Statement
final case class ImportFrom(module: Option[Ident], names: Seq[Alias], level: Option[Int]) extends Statement
final case class Global(names: Seq[Ident]) extends Statement
final case class Nonlocal(names: Seq[Ident]) extends Statement
final case class ExprStatement(expr: Expr) extends Statement
final case class Pass() extends Statement
final case class Break() extends Statement
final case class Continue() extends Statement

sealed trait Expr extends Tree
final case class BoolOp(op: BoolOperator, values: Seq[Expr]) extends Expr
final case class NamedExpr(target: Expr, value: Expr) extends Expr
final case class BinOp(left: Expr, op: Operator, right: Expr) extends Expr
final case class UnaryOp(op: UnaryOperator, operand: Expr) extends Expr
final case class Lambda(args: Arguments, body: Expr) extends Expr
final case class IfExp(test: Expr, body: Expr, orElse: Expr) extends Expr
final case class Dict(keys: Seq[Expr], values: Seq[Expr]) extends Expr
final case class ConstructSet(elts: Seq[Expr]) extends Expr
final case class ListComp(elt: Expr, generators: Seq[Comprehension]) extends Expr
final case class SetComp(elt: Expr, generators: Seq[Comprehension]) extends Expr
final case class DictComp(key: Expr, value: Expr, generators: Seq[Comprehension]) extends Expr
final case class GeneratorExp(elt: Expr, generators: Seq[Comprehension]) extends Expr

final case class Await(value: Expr) extends Expr
final case class Yield(value: Option[Expr]) extends Expr
final case class YieldFrom(value: Expr) extends Expr

final case class Compare(left: Expr, ops: Seq[ComparisonOperator], comparators: Seq[Expr]) extends Expr
final case class Call(func: Expr, args: Seq[Expr], keywords: Seq[Keyword]) extends Expr
final case class FormattedValue(value: Expr, conversion: Option[Int], formatSpec: Option[Expr]) extends Expr
final case class JoinedStr(values: Seq[Expr]) extends Expr
final case class Constant[A](value: Literal[A]) extends Expr

final case class Attribute(value: Expr, attr: Ident, ctx: ExprContext) extends Expr
final case class Subscript(value: Expr, slice: Expr, ctx: ExprContext) extends Expr
final case class Starred(value: Expr, ctx: ExprContext) extends Expr
final case class Name(id: Ident, ctx: ExprContext) extends Expr
final case class ConstructList(elts: Seq[Expr], ctx: ExprContext) extends Expr
final case class ConstructTuple(elts: Seq[Expr], ctx: ExprContext) extends Expr

final case class Slice(lower: Option[Expr], upper: Option[Expr], step: Option[Expr]) extends Expr

sealed trait Literal[A] {
  def value: A
}
final case class StringLiteral(value: String, flags: Option[String] = None) extends Literal[String]
final case class BytesLiteral(value: String, flags: String) extends Literal[String]
final case class BooleanLiteral(value: Boolean) extends Literal[Boolean]
case object NoneLiteral extends Literal[None.type] {
  val value: None.type = None
}

sealed abstract class NumericLiteral[A <: Number : Numeric] extends Literal[A] {
  def numeric: Numeric[A] = implicitly
}
final case class LongLiteral(value: BigInt) extends NumericLiteral[BigInt]
final case class IntegerLiteral(value: BigInt) extends NumericLiteral[BigInt]
final case class FloatLiteral(value: BigDecimal) extends NumericLiteral[BigDecimal]
final case class ImaginaryLiteral(value: BigDecimal) extends NumericLiteral[BigDecimal]

case object Ellipsis extends Literal[String] {
  val value: String = "..."
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

sealed trait BoolOperator
case object And extends BoolOperator
case object Or extends BoolOperator

sealed abstract class Operator(tok: String) {
  import fastparse.P
  def parser[_ : P]: P[Operator] = P(tok).map(_ => this)
  def mk(l: Expr, r: Expr): Expr = BinOp(l, this, r)
}

case object Add extends Operator("+")
case object Sub extends Operator("-")
case object Mult extends Operator("*")
case object MatMult extends Operator("@")
case object Div extends Operator("/")
case object Mod extends Operator("%")
case object Pow extends Operator("**")
case object LShift extends Operator("<<")
case object RShift extends Operator(">>")
case object BitOr extends Operator("|")
case object BitXor extends Operator("^")
case object BitAnd extends Operator("&")
case object FloorDiv extends Operator("//")

sealed trait UnaryOperator
case object Invert extends UnaryOperator
case object Not extends UnaryOperator
case object UAdd extends UnaryOperator
case object USub extends UnaryOperator

sealed trait ComparisonOperator
case object Eq extends ComparisonOperator
case object NotEq extends ComparisonOperator
case object Lt extends ComparisonOperator
case object LtE extends ComparisonOperator
case object Gt extends ComparisonOperator
case object GtE extends ComparisonOperator
case object Is extends ComparisonOperator
case object IsNot extends ComparisonOperator
case object In extends ComparisonOperator
case object NotIn extends ComparisonOperator

final case class Comprehension(target: Expr, iter: Expr, ifs: Seq[Expr], isAsync: Int) extends Tree

final case class ExceptHandler(typ: Option[Expr], name: Option[Ident], body: Seq[Statement]) extends Tree

final case class Arguments(
  posOnlyArgs: Seq[Arg],
  args: Seq[Arg],
  varArg: Option[Arg],
  kwOnlyArgs: Seq[Arg],
  kwDefaults: Seq[Option[Expr]],
  kwArg: Option[Arg],
  defaults: Seq[Expr]
) extends Tree

object Arguments {
  import Util._
  val empty: Arguments = Arguments(Nil, Nil, None, Nil, Nil, None, Nil)
  def fromParams(posOnly: Seq[Param], params: Seq[Param], kws: Option[KWOnlyParams]): Arguments = {
    val kwOnlyArgs = kws.toSeq.flatMap(_.kwOnlyParams)
    Arguments(
      posOnlyArgs = posOnly.toArgs,
      args = params.toArgs,
      varArg = kws.flatMap(_.vararg.map(_.arg)),
      kwOnlyArgs = kwOnlyArgs.toArgs,
      kwDefaults = kwOnlyArgs.paddedDefaults,
      kwArg = kws.flatMap(_.kwParam.map(_.arg)),
      defaults = posOnly.defaults ++ params.defaults
    )
  }

  implicit class OptionOps(val self: Option[Arguments]) extends AnyVal {
    def orEmpty: Arguments = self.getOrElse(empty)
  }
}

final case class Arg(arg: Ident, annotation: Option[Expr], typeComment: Option[String]) extends Tree
object Arg {
  def name(name: Ident): Arg = Arg(name, None, None)
}

final case class Keyword(arg: Option[Ident], value: Expr) extends Tree

final case class Alias(name: Ident, asName: Option[Ident]) extends Tree

final case class WithItem(contentExpr: Expr, optionalVars: Option[Expr]) extends Tree

sealed trait Position {
  def offset: Int
  def start: Int
  def end: Int
}

final case class RangePosition(start: Int, end: Int) extends Position {
  def offset: Int = start
}