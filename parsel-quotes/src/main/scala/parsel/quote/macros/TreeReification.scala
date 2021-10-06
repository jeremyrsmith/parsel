package parsel
package quote.macros

import parsel.ast._

import scala.reflect.macros.whitebox

abstract class TreeReification(val c: whitebox.Context) {
  import c.universe.{Assign => _, Return => _, Constant => _, Literal => _, Try => _, If => _, Import => _, ClassDef => _, Tree => ScalaTree, Expr => ScalaExpr, _}

  def reifyTree(c: whitebox.Context)(tree: Tree): ScalaTree = {
    tree match {
      case Pass() => q"_root_.parsel.ast.Pass()"
      case Break() => q"_root_.parsel.ast.Break()"
      case Continue() => q"_root_.parsel.ast.Continue()"
      case Name(name) => q"_root_.parsel.ast.Name($name)"
      case Constant(value) => q"_root_.parsel.ast.Constant(${reifyLiteral(c)(value)})"
      case Module(body) => q"_root_.parsel.ast.Module(${seq(body)})"
      case Interactive(body) => q"_root_.parsel.ast.Interactive(${seq(body)})"
      case Expression(expr) => q"_root_.parsel.ast.Expression(${r(expr)})"
      case Return(value) => q"_root_.parsel.ast.Return(${opt(value)})"
      case Raise(exc, cause) => q"_root_.parsel.ast.Raise(${opt(exc)}, ${opt(cause)})"
      case Dict(keys, values) => q"_root_.parsel.ast.Dict(${seq(keys)}, ${seq(values)})"
      case ConstructSet(elts) => q"_root_.parsel.ast.ConstructSet(${seq(elts)})"
      case Yield(value) => q"_root_.parsel.ast.Yield(${opt(value)})"
      case ConstructList(elts, ctx) => q"_root_.parsel.ast.ConstructList(${seq(elts)}, ${ec(ctx)})"
      case ConstructTuple(elts, ctx) => q"_root_.parsel.ast.ConstructTuple(${seq(elts)}, ${ec(ctx)})"
      case Slice(lower, upper, step) => q"_root_.parsel.ast.Slice(${opt(lower)}, ${opt(upper)}, ${opt(step)})"
      case Arguments(posOnlyArgs, args, varArg, kwOnlyArgs, kwDefaults, kwArg, defaults) => q"_root_.parsel.ast.Arguments(${seq(posOnlyArgs)}, ${seq(args)}, ${opt(varArg)}, ${seq(kwOnlyArgs)}, ${seqOpt(kwDefaults)}, ${opt(kwArg)}, ${seq(defaults)})"
      case Delete(targets) => q"_root_.parsel.ast.Delete(${seq(targets)})"
      case Assign(targets, value, typeComment) => q"_root_.parsel.ast.Assign(${seq(targets)}, ${r(value)}, $typeComment)"
      case AugAssign(target, operator, value) => q"_root_.parsel.ast.AugAssign(${r(target)}, ${op(operator)}, ${r(value)})"
      case AnnAssign(target, annotation, value, simple) => q"_root_.parsel.ast.AnnAssign(${r(target)}, ${r(annotation)}, ${opt(value)}, $simple)"
      case For(target, iter, body, orElse, typeComment) => q"_root_.parsel.ast.For(${r(target)}, ${r(iter)}, ${seq(body)}, ${seq(orElse)}, $typeComment)"
      case AsyncFor(target, iter, body, orElse, typeComment) => q"_root_.parsel.ast.AsyncFor(${r(target)}, ${r(iter)}, ${seq(body)}, ${seq(orElse)}, $typeComment)"
      case While(test, body, orElse) => q"_root_.parsel.ast.While(${r(test)}, ${seq(body)}, ${seq(orElse)})"
      case If(test, body, orElse) => q"_root_.parsel.ast.If(${r(test)}, ${seq(body)}, ${seq(orElse)})"
      case With(items, body, typeComment) => q"_root_.parsel.ast.With(${seq(items)}, ${seq(body)}, $typeComment)"
      case AsyncWith(items, body, typeComment) => q"_root_.parsel.ast.AsyncWith(${seq(items)}, ${seq(body)}, $typeComment)"
      case Try(body, exceptHandlers, orElse, finalBody) => q"_root_.parsel.ast.Try(${seq(body)}, ${seq(exceptHandlers)}, ${seq(orElse)}, ${seq(finalBody)})"
      case Assert(test, msg) => q"_root_.parsel.ast.Assert(${r(test)}, ${opt(msg)})"
      case Import(names) => q"_root_.parsel.ast.Import(${seq(names)})"
      case ImportFrom(module, names, level) => q"_root_.parsel.ast.ImportFrom(${opt(module)}, ${seq(names)}, $level)"
      case Global(names) => q"_root_.parsel.ast.Global(${seq(names)})"
      case Nonlocal(names) => q"_root_.parsel.ast.Nonlocal(${seq(names)})"
      case ExprStatement(expr) => q"_root_.parsel.ast.ExprStatement(${r(expr)})"
      case BoolOp(op, values) => q"_root_.parsel.ast.BoolOp(${boolOp(op)}, ${seq(values)})"
      case NamedExpr(target, value) => q"_root_.parsel.ast.NamedExpr(${r(target)}, ${r(value)})"
      case BinOp(left, bop, right) => q"_root_.parsel.ast.BinOp(${r(left)}, ${op(bop)}, ${r(right)})"
      case UnaryOp(op, operand) => q"_root_.parsel.ast.UnaryOp(${uOp(op)}, ${r(operand)})"
      case Lambda(args, body) => q"_root_.parsel.ast.Lambda(${r(args)}, ${r(body)})"
      case IfExp(body, test, orElse) => q"_root_.parsel.ast.IfExp(${r(body)}, ${r(test)}, ${r(orElse)})"
      case ListComp(elt, generators) => q"_root_.parsel.ast.ListComp(${r(elt)}, ${seq(generators)})"
      case SetComp(elt, generators) => q"_root_.parsel.ast.SetComp(${r(elt)}, ${seq(generators)})"
      case DictComp(key, value, generators) => q"_root_.parsel.ast.DictComp(${r(key)}, ${r(value)}, ${seq(generators)})"
      case GeneratorExp(elt, generators) => q"_root_.parsel.ast.GeneratorExp(${r(elt)}, ${seq(generators)})"
      case Await(value) => q"_root_.parsel.ast.Await(${r(value)})"
      case YieldFrom(value) => q"_root_.parsel.ast.YieldFrom(${r(value)})"
      case Compare(left, ops, comparators) => q"_root_.parsel.ast.Compare(${r(left)}, ${compareOps(ops)}, ${seq(comparators)})"
      case Call(func, args, keywords) => q"_root_.parsel.ast.Call(${r(func)}, ${seq(args)}, ${seq(keywords)})"
      case FormattedValue(value, conversion, formatSpec) => q"_root_.parsel.ast.FormattedValue(${r(value)}, $conversion, ${opt(formatSpec)})"
      case JoinedStr(values) => q"_root_.parsel.ast.JoinedStr(${seq(values)})"
      case Attribute(value, attr, ctx) => q"_root_.parsel.ast.Attribute(${r(value)}, ${r(attr)}, ${ec(ctx)})"
      case Subscript(value, slice, ctx) => q"_root_.parsel.ast.Subscript(${r(value)}, ${r(slice)}, ${ec(ctx)})"
      case Starred(value, ctx) => q"_root_.parsel.ast.Starred(${r(value)}, ${ec(ctx)})"
      case Comprehension(target, iter, ifs, isAsync) => q"_root_.parsel.ast.Comprehension(${r(target)}, ${r(iter)}, ${seq(ifs)}, $isAsync)"
      case ExceptHandler(typ, name, body) => q"_root_.parsel.ast.ExceptHandler(${opt(typ)}, ${opt(name)}, ${seq(body)})"
      case Arg(arg, annotation, typeComment) => q"_root_.parsel.ast.Arg(${r(arg)}, ${opt(annotation)}, $typeComment)"
      case Keyword(arg, value) => q"_root_.parsel.ast.Keyword(${opt(arg)}, ${r(value)})"
      case Alias(name, asName) => q"_root_.parsel.ast.Alias(${r(name)}, ${opt(asName)})"
      case WithItem(contentExpr, optionalVars) => q"_root_.parsel.ast.WithItem(${r(contentExpr)}, ${opt(optionalVars)})"
      case FunctionDef(name, args, body, decoratorList, returns, typeComment) => q"_root_.parsel.ast.FunctionDef(${r(name)}, ${r(args)}, ${seq(body)}, ${seq(decoratorList)}, ${opt(returns)}, $typeComment)"
      case AsyncFunctionDef(name, args, body, decoratorList, returns, typeComment) => q"_root_.parsel.ast.AsyncFunctionDef(${r(name)}, ${r(args)}, ${seq(body)}, ${seq(decoratorList)}, ${opt(returns)}, $typeComment)"
      case ClassDef(name, bases, keywords, body, decoratorList) => q"_root_.parsel.ast.ClassDef(${r(name)}, ${seq(bases)}, ${seq(keywords)}, ${seq(body)}, ${seq(decoratorList)})"
    }
  }


  private def r(tree: Tree): ScalaTree = reifyTree(c)(tree)
  private def opt(tree: Option[Tree]): ScalaTree = tree match {
    case Some(tree) => q"_root_.scala.Some(${r(tree)})"
    case None => q"_root_.scala.None"
  }
  private def seqTrees(trees: Seq[ScalaTree]): ScalaTree = q"Seq(..$trees)"
  private def seq(trees: Seq[Tree]): ScalaTree = seqTrees(trees.map(reifyTree(c)))
  private def seqOpt(trees: Seq[Option[Tree]]): ScalaTree = seqTrees(trees.map(t => opt(t)))
  private def compareOps(ops: Seq[ComparisonOperator]): ScalaTree = seqTrees {
    ops.map {
      case Eq    => reify(Eq).tree
      case NotEq => reify(NotEq).tree
      case Lt    => reify(Lt).tree
      case LtE   => reify(LtE).tree
      case Gt    => reify(Gt).tree
      case GtE   => reify(GtE).tree
      case Is    => reify(Is).tree
      case IsNot => reify(IsNot).tree
      case In    => reify(In).tree
      case NotIn => reify(NotIn).tree
    }
  }


  private def ec[A](ctx: ExprContext): ScalaTree = ctx match {
    case Load => reify(Load).tree
    case Store => reify(Store).tree
    case Del => reify(Del).tree
  }

  private def op(operator: Operator): ScalaExpr[Operator] = operator match {
    case Add      => reify(Add)
    case Sub      => reify(Sub)
    case Mult     => reify(Mult)
    case MatMult  => reify(MatMult)
    case Div      => reify(Div)
    case Mod      => reify(Mod)
    case Pow      => reify(Pow)
    case LShift   => reify(LShift)
    case RShift   => reify(RShift)
    case BitOr    => reify(BitOr)
    case BitXor   => reify(BitXor)
    case BitAnd   => reify(BitAnd)
    case FloorDiv => reify(FloorDiv)
  }

  private def boolOp(op: BoolOperator): ScalaExpr[BoolOperator] = op match {
    case And => reify(And)
    case Or  => reify(Or)
  }

  private def uOp(op: UnaryOperator): ScalaExpr[UnaryOperator] = op match {
    case Invert => reify(Invert)
    case Not    => reify(Not)
    case UAdd   => reify(UAdd)
    case USub   => reify(USub)
  }

  private def reifyLiteral[A](c: whitebox.Context)(literal: Literal[A]): ScalaTree =
    literal match {
      case StringLiteral(value, flags) => q"_root_.parsel.ast.StringLiteral($value, $flags)"
      case BytesLiteral(value, flags) => q"_root_.parsel.ast.BytesLiteral($value, $flags)"
      case BooleanLiteral(value) => q"_root_.parsel.ast.BooleanLiteral($value)"
      case NoneLiteral => q"_root_.parsel.ast.NoneLiteral"
      case LongLiteral(value) => q"_root_.parsel.ast.LongLiteral(_root_.scala.math.BigInt(${value.toString()}))"
      case IntegerLiteral(value) => q"_root_.parsel.ast.IntegerLiteral(_root_.scala.math.BigInt(${value.toString()}))"
      case FloatLiteral(value) => q"_root_.parsel.ast.FloatLiteral(_root_.scala.math.BigDecimal(${value.toString()}))"
      case ImaginaryLiteral(value) => q"_root_.parsel.ast.ImaginaryLiteral(_root_.scala.math.BigDecimal(${value.toString()}))"
      case Ellipsis => q"_root_.parsel.ast.Ellipsis"
    }


}
