package polynote.python.parse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFreeSpec with Matchers {

  "assignments" in {
    val result = new Parser(
      """x = 10
        |x,y = 10
        |""".stripMargin
    ).parse()
    println(result)
  }

  "test" in {
    val result = new Parser(bigFile).parse()
    println(result)
//    val lines = Parser.parseLines(bigFile)
//    println(lines.map(_.asString).mkString("\n"))
  }

}
