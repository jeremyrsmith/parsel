package parsel.parse

case class ParseError(msg: String, offset: Int, line: Int, col: Int, lineStr: String) extends Throwable(
  s"Syntax error: $msg ($line:$col)\n" +
    lineStr + "\n" +
    " " * (col - 1) + "^"
)