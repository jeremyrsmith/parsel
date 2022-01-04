package parsel.ast

import parsel.ast.Util.{Param, Params}

trait TreeTransformer extends ExprTransformer {

  def transformComplete(tree: CompleteTree): CompleteTree = tree match {
    case mod: Module => transformModule(mod)
    case expr: Expr => transformExpr(expr)
    case statement: Statement => transformStatement(statement)
  }

  def transform(tree: Tree): Tree = tree match {
    case mod: Module => transformModule(mod)
    case statement: Statement => transformStatement(statement)
    case expr: Expr => transformExpr(expr)
    case comprehension: Comprehension => transformComprehension(comprehension)
    case exceptHandler: ExceptHandler => transformExceptHandler(exceptHandler)
    case arguments: Arguments => transformArguments(arguments)
    case arg: Arg => transformParam(Param(arg.arg, arg.annotation, None)).arg
    case keyword: Keyword => transformKeyword(keyword)
    case alias: Alias => transformAlias(alias)
    case withItem: WithItem => transformWithItem(withItem)
  }

  def transformModule(module: Module): Module = Module(transformStatements(module.body))

  def transformStatement(statement: Statement): Statement = statement match {
    case fd: FunctionDef => transformFunctionDef(fd)
    case afd: AsyncFunctionDef => transformAsyncFunctionDef(afd)
    case cd: ClassDef => transformClassDef(cd)
    case Return(value) => Return(value.map(transformExpr))
    case Delete(targets) => Delete(transformExprs(targets))
    case Assign(targets, value, typeComment) => Assign(transformExprs(targets), transformExpr(value), typeComment.map(transformTypeComment))
    case AugAssign(target, operator, value) => AugAssign(transformExpr(target), operator, transformExpr(value))
    case AnnAssign(target, annotation, value, simple) =>
      val lhs = transformExpr(target)
      AnnAssign(lhs, transformTypeExpr(annotation), value.map(transformExpr), if (!lhs.isInstanceOf[Name]) 0 else simple)
    case For(target, iter, body, orElse, typeComment) => For(transformExpr(target), transformExpr(iter), transformStatements(body), transformStatements(orElse), typeComment.map(transformTypeComment))
    case AsyncFor(target, iter, body, orElse, typeComment) => AsyncFor(transformExpr(target), transformExpr(iter), transformStatements(body), transformStatements(orElse), typeComment.map(transformTypeComment))
    case While(test, body, orElse) => While(transformExpr(test), transformStatements(body), transformStatements(orElse))
    case If(test, body, orElse) => If(transformExpr(test), transformStatements(body), transformStatements(orElse))
    case With(items, body, typeComment) => With(transformWithItems(items), transformStatements(body), typeComment.map(transformTypeComment))
    case AsyncWith(items, body, typeComment) => AsyncWith(transformWithItems(items), transformStatements(body), typeComment.map(transformTypeComment))
    case Raise(exc, cause) => Raise(exc.map(transformExpr), cause.map(transformExpr))
    case Try(body, exceptHandlers, orElse, finalBody) => Try(transformStatements(body), transformExceptHandlers(exceptHandlers), transformStatements(orElse), transformStatements(finalBody))
    case Assert(test, msg) => Assert(transformExpr(test), msg.map(transformExpr))
    case Import(names) => Import(transformAliases(names))
    case ImportFrom(module, names, level) => ImportFrom(module.map(transformName), transformAliases(names), level)
    case Global(names) => Global(transformNames(names))
    case Nonlocal(names) => Nonlocal(transformNames(names))
    case ExprStatement(expr) => ExprStatement(transformExpr(expr))
    case stat@(Pass() | Break() | Continue()) => stat
  }

  def transformStatements(statements: Seq[Statement]): Seq[Statement] = statements.map(transformStatement) match {
    case stats if stats.size > 1 => stats.filterNot(_.isInstanceOf[Pass])
    case stats => stats
  }

  def transformDecorator(decorator: Expr): Expr = decorator
  def transformDecorators(decorators: Seq[Expr]): Seq[Expr] = decorators.map(transformDecorator)

  def transformNames(names: Seq[Name]): Seq[Name] = names.map(transformName)

  def transformTypeComment(typeComment: String): String = typeComment

  def transformFunctionDef(tree: FunctionDef): FunctionDef = FunctionDef(
    transformName(tree.name),
    transformArguments(tree.args),
    transformStatements(tree.body),
    transformDecorators(tree.decoratorList),
    tree.returns.map(transformTypeExpr),
    tree.typeComment.map(transformTypeComment)
  )

  def transformAsyncFunctionDef(tree: AsyncFunctionDef): AsyncFunctionDef = transformFunctionDef(tree.toFunctionDef).toAsyncFunctionDef

  def transformClassDef(classDef: ClassDef): ClassDef = classDef match {
    case ClassDef(name, bases, keywords, body, decoratorList) => ClassDef(
      transformName(name),
      bases.map(transformTypeExpr),
      transformKeywords(keywords),
      transformStatements(body),
      transformDecorators(decoratorList)
    )
  }

  def transformWithItems(items: Seq[WithItem]): Seq[WithItem] = items.map(transformWithItem)
  def transformWithItem(item: WithItem): WithItem = WithItem(transformExpr(item.contentExpr), item.optionalVars.map(transformExpr))

  def transformExceptHandlers(handlers: Seq[ExceptHandler]): Seq[ExceptHandler] = handlers.map(transformExceptHandler)
  def transformExceptHandler(handler: ExceptHandler): ExceptHandler = ExceptHandler(handler.typ.map(transformTypeExpr), handler.name.map(transformName), transformStatements(handler.body))

  def transformAliases(aliases: Seq[Alias]): Seq[Alias] = aliases.map(transformAlias)
  def transformAlias(alias: Alias): Alias = Alias(transformName(alias.name), alias.asName.map(transformName))

}

trait ExprTransformer {


  def transformExpr(expr: Expr): Expr = expr match {
    case name: Name => transformName(name)
    case BoolOp(op, values) => BoolOp(op, transformExprs(values))
    case NamedExpr(target, value) => NamedExpr(transformExpr(target), transformExpr(value))
    case BinOp(left, op, right) => BinOp(transformExpr(left), op, transformExpr(right))
    case UnaryOp(op, operand) => UnaryOp(op, transformExpr(operand))
    case Lambda(args, body) => Lambda(transformArguments(args), transformExpr(body))
    case IfExp(body, test, orElse) => IfExp(transformExpr(body), transformExpr(test), transformExpr(orElse))
    case Dict(keys, values) => Dict(transformExprs(keys), transformExprs(values))
    case ConstructSet(elts) => ConstructSet(transformExprs(elts))
    case ListComp(elt, generators) => ListComp(transformExpr(elt), transformComprehensions(generators))
    case SetComp(elt, generators) => SetComp(transformExpr(elt), transformComprehensions(generators))
    case DictComp(key, value, generators) => DictComp(transformExpr(key), transformExpr(value), transformComprehensions(generators))
    case GeneratorExp(elt, generators) => GeneratorExp(transformExpr(elt), transformComprehensions(generators))
    case Await(value) => Await(transformExpr(value))
    case Yield(value) => Yield(value.map(transformExpr))
    case YieldFrom(value) => YieldFrom(transformExpr(value))
    case Compare(left, ops, comparators) => Compare(transformExpr(left), ops, transformExprs(comparators))
    case Call(func, args, keywords) => Call(transformExpr(func), transformExprs(args), transformKeywords(keywords))
    case FormattedValue(value, conversion, formatSpec) => FormattedValue(transformExpr(value), conversion, formatSpec.map(transformFormatSpec))
    case joinedStr: JoinedStr => transformJoinedStr(joinedStr)
    case c: Constant[_] => c
    case Attribute(value, attr, ctx) => Attribute(transformExpr(value), transformName(attr), transformExprContext(ctx))
    case Subscript(value, slice, ctx) => Subscript(transformExpr(value), transformExpr(slice), transformExprContext(ctx))
    case Starred(value, ctx) => Starred(transformExpr(value), transformExprContext(ctx))
    case ConstructList(elts, ctx) => ConstructList(transformExprs(elts), transformExprContext(ctx))
    case ConstructTuple(elts, ctx) => ConstructTuple(transformExprs(elts), transformExprContext(ctx))
    case Slice(lower, upper, step) => Slice(lower.map(transformExpr), upper.map(transformExpr), step.map(transformExpr))
  }

  def transformExprs(exprs: Seq[Expr]): Seq[Expr] = exprs.map(transformExpr)

  def transformTypeExpr(expr: Expr): Expr = transformExpr(expr)

  def transformName(name: Name): Name = name

  def transformArguments(args: Arguments): Arguments = {
    def paramMaybeDefault(tuple: (Arg, Option[Expr])): Param = Param(tuple._1.arg, tuple._1.annotation, tuple._2)
    def paramNoDefault(arg: Arg): Param = Param(arg.arg, arg.annotation, None)

    val params = args match {
      case Arguments(posOnly, args, varArg, kwOnly, kwDefaults, kwArg, defaults) =>
        val paddedDefaults = Seq.fill((posOnly.size + args.size) - defaults.size)(None) ++ defaults.map(Some(_))
        val posOnlyParams = posOnly.zip(paddedDefaults.take(posOnly.size)).map(paramMaybeDefault)
        val normalParams = args.zip(paddedDefaults.drop(posOnly.size)).map(paramMaybeDefault)

        val kwParams = kwOnly.zip(Seq.fill(kwOnly.size - kwDefaults.size)(None) ++ kwDefaults).map {
          case (arg, default) => Param(arg.arg, arg.annotation, default)
        }

        Params(posOnlyParams, normalParams, varArg.map(paramNoDefault), kwParams, kwArg.map(paramNoDefault))
    }
    Arguments.fromParams(transformParams(params))
  }

  def transformParams(params: Params): Params = params match {
    case Params(posOnly, args, varArg, kwOnly, kw) => Params(
      posOnly.map(transformPosOnlyParam),
      args.map(transformParam),
      varArg.map(transformVarArgParam),
      kwOnly.map(transformKwOnlyParam),
      kw.map(transformKwArgParam)
    )
  }

  def transformPosOnlyParam(param: Param): Param = transformParam(param)
  def transformKwOnlyParam(param: Param): Param = transformParam(param)
  def transformVarArgParam(param: Param): Param = transformParam(param)
  def transformKwArgParam(param: Param): Param = transformParam(param)
  def transformParam(param: Param): Param = Param(transformName(param.name), param.typ.map(transformTypeExpr), param.default.map(transformExpr))

  def transformComprehension(comprehension: Comprehension): Comprehension = Comprehension(
    target = transformExpr(comprehension.target),
    iter = transformExpr(comprehension.iter),
    ifs = transformExprs(comprehension.ifs),
    isAsync = comprehension.isAsync
  )

  def transformComprehensions(generators: Seq[Comprehension]): Seq[Comprehension] = generators.map(transformComprehension)

  def transformKeyword(keyword: Keyword): Keyword = Keyword(
    keyword.arg.map(transformName),
    transformExpr(keyword.value)
  )

  def transformKeywords(keywords: Seq[Keyword]): Seq[Keyword] = keywords.map(transformKeyword)

  def transformJoinedStr(joinedStr: JoinedStr): JoinedStr = JoinedStr(transformExprs(joinedStr.values))

  def transformFormatSpec(formatSpec: JoinedStr): JoinedStr = transformJoinedStr(formatSpec)

  def transformExprContext(ctx: ExprContext): ExprContext = ctx
}