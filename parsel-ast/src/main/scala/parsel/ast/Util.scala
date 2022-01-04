package parsel.ast

object Util {

  case class KWParam(param: Param) extends AnyVal

  case class Param(name: Name, typ: Option[Expr], default: Option[Expr]) {
    val arg: Arg = Arg(name, typ, None) // TODO: TYPE_COMMENT
  }

  object Param {
    implicit class Ops(private val self: Seq[Param]) extends AnyVal {
      def defaults: Seq[Expr] = self.collect {
        case Param(_, _, Some(expr)) => expr
      }

      def paddedDefaults: Seq[Option[Expr]] = self.map(_.default)

      def toArgs: Seq[Arg] = self.map(_.arg)
    }
  }

  case class Params(
    posOnlyParams: Seq[Param],
    params: Seq[Param],
    varArg: Option[Param],
    kwOnlyParams: Seq[Param],
    kwParam: Option[Param]
  ) {
    def hasKws: Boolean = varArg.nonEmpty || kwOnlyParams.nonEmpty || kwParam.nonEmpty
    def toPosOnly: Params = copy(posOnlyParams = params, params = Seq.empty)
    def withParam(param: Param): Params = copy(params = params :+ param)
    def withVarArg(name: String): Params = withVarArg(Param(Name(name), None, None))
    def withVarArg(param: Param): Params = copy(varArg = Some(param))
    def withKwOnlyParam(param: Param): Params = copy(kwOnlyParams = kwOnlyParams :+ param)
    def withKwParam(name: String): Params = withKwParam(Param(Name(name), None, None))
    def withKwParam(param: Param): Params = copy(kwParam = Some(param))
  }

  object Params {
    val empty: Params = Params(Seq.empty, Seq.empty, None, Seq.empty, None)
  }

  def normalizeFlags(flags: String): String = flags.toLowerCase.replace("r", "")
}
