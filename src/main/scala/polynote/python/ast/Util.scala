package polynote.python.ast

object Util {

  case class KWParam(param: Param) extends AnyVal

  case class Param(name: Ident, typ: Option[Expr], default: Option[Expr]) {
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

  case class KWOnlyParams(vararg: Option[Param], kwOnlyParams: Seq[Param], kwParam: Option[Param]) {
    def nonEmpty: Boolean = vararg.nonEmpty || kwOnlyParams.nonEmpty || kwParam.nonEmpty
  }

}
