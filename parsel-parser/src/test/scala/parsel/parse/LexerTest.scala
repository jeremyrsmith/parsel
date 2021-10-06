package parsel.parse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LexerTest extends AnyFreeSpec with Matchers {
  import Lexer._

  "peekLineRootTokens" in {

    val lines =
      s"""x = (52 + 11) / 2
         |y.foo(bar, baz)
         |""".stripMargin

    val lexer = new Lexer(lines, ignoreWhitespace = true)
    lexer.peekLineRootTokens shouldEqual Seq(Indent(0), Word("x"), Operator("="), Operator("/"), IntegerNum(BigInt(2), "2"))

  }

}
