package polynote.python

import fastparse._

package object parse {
  def failMsg(msg: => String)(implicit ctx: P[_]): P[Nothing] = {
    val res = ctx.freshFailure()
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => msg)
    res
  }

  case class ParseError(msg: String, line: Int, char: Int, pos: Int) extends RuntimeException(s"$msg ($line:$char)")
}
