package parsel.parse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import parsel.ast._

class ExpressionParserTest extends AnyFreeSpec with Matchers {

  def test(str: String): Expr = ExpressionParser.parse(str)

  "numbers" in {
    test("2") shouldEqual Constant(IntegerLiteral(BigInt(2)))
    test("2.") shouldEqual Constant(FloatLiteral(BigDecimal("2.0")))
    test("2.1") shouldEqual Constant(FloatLiteral(BigDecimal("2.1")))
    test(".1") shouldEqual Constant(FloatLiteral(BigDecimal("0.1")))
    test(".1e5") shouldEqual Constant(FloatLiteral(BigDecimal("0.1e5")))
    test(".1e5j") shouldEqual Constant(ImaginaryLiteral(BigDecimal("0.1e5")))
    test("0xFEA") shouldEqual Constant(IntegerLiteral(BigInt("FEA", 16)))
    test("1e5") shouldEqual Constant(FloatLiteral(BigDecimal("1e5")))
    test("1.1e5") shouldEqual Constant(FloatLiteral(BigDecimal("1.1e5")))
    test("1.1j") shouldEqual Constant(ImaginaryLiteral(BigDecimal("1.1")))
  }

  "math expressions" in {
    test("x + y / 2 * 3 - 4") shouldEqual BinOp(BinOp(Name("x"), Add, BinOp(BinOp(Name("y"), Div, Constant(IntegerLiteral(2))), Mult, Constant(IntegerLiteral(3)))), Sub, Constant(IntegerLiteral(4)))
  }

  "boolean expressions" in {
    test("foo or bar and baz") shouldEqual BoolOp(Or, Seq(Name("foo"), BoolOp(And, Seq(Name("bar"), Name("baz")))))
  }

  // TODO: more stuff here

}
