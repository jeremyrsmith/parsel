package parsel.parse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.propBoolean
import parsel.ast._

import scala.util.control.NonFatal

class ExpressionParserTest extends AnyFreeSpec with Matchers with Checkers {

  def test(str: String): Expr = ExpressionParser.parse(str)

  "numbers" in {
    test("0.3902823103720054610353374055478757") shouldEqual Constant(FloatLiteral(BigDecimal("0.3902823103720054610353374055478757")))
    test("2") shouldEqual Constant(IntegerLiteral(BigInt(2)))
    test("2.") shouldEqual Constant(FloatLiteral(BigDecimal("2.0")))
    test("2.1") shouldEqual Constant(FloatLiteral(BigDecimal("2.1")))
    test(".1") shouldEqual Constant(FloatLiteral(BigDecimal("0.1")))
    test(".1e5") shouldEqual Constant(FloatLiteral(BigDecimal("0.1e5")))
    test(".1e5j") shouldEqual Constant(ImaginaryLiteral(BigDecimal("0.1e5")))
    test("0xFEA") shouldEqual Constant(IntegerLiteral(BigInt("FEA", 16)))
    test("1e5") shouldEqual Constant(FloatLiteral(BigDecimal("1e5")))
    test("1.1e5") shouldEqual Constant(FloatLiteral(BigDecimal("1.1e5")))
    test("1.1e+5") shouldEqual Constant(FloatLiteral(BigDecimal("1.1e5")))
    test("1.1e-5") shouldEqual Constant(FloatLiteral(BigDecimal("1.1e-5")))
    test("1.1j") shouldEqual Constant(ImaginaryLiteral(BigDecimal("1.1")))
    test("0j") shouldEqual Constant(ImaginaryLiteral(BigDecimal(0)))
  }

  "strings" in {
    test("b''") shouldEqual Constant(BytesLiteral("", "b"))
  }

  "math expressions" in {
    test("x + y / 2 * 3 - 4") shouldEqual BinOp(BinOp(Name("x"), Add, BinOp(BinOp(Name("y"), Div, Constant(IntegerLiteral(2))), Mult, Constant(IntegerLiteral(3)))), Sub, Constant(IntegerLiteral(4)))
  }

  "boolean expressions" in {
    test("foo or bar and baz") shouldEqual BoolOp(Or, Seq(Name("foo"), BoolOp(And, Seq(Name("bar"), Name("baz")))))
  }

  "edge case unary ops" in {
    test("~2.0789714070126335030989024388363104782E-185j") shouldEqual UnaryOp(Invert, Constant(ImaginaryLiteral(BigDecimal("2.0789714070126335030989024388363104782E-185"))))
  }

  "assorted failures from checks" in {
    test("f'{ 0.5124107j!r }'") shouldEqual JoinedStr(Seq(FormattedValue(Constant(ImaginaryLiteral(BigDecimal("0.5124107"))), Some(114), None)))
    test("(_ := 2.125621877610000947967746661424240E+292)") shouldEqual NamedExpr(Name("_"), Constant(FloatLiteral(BigDecimal("2.125621877610000947967746661424240E+292"))))
    test("_[::gf]") shouldEqual Subscript(Name("_"), Slice(None, None, Some(Name("gf"))), Load)
    test("b[_:b'':0.4918896]") shouldEqual Subscript(Name("b"), Slice(Some(Name("_")), Some(Constant(BytesLiteral("", "b"))), Some(Constant(FloatLiteral(BigDecimal("0.4918896"))))), Load)
    test("b''[_:'']") shouldEqual Subscript(Constant(BytesLiteral("", "b")), Slice(Some(Name("_")), Some(Constant(StringLiteral("", None))), None), Load)
  }

  "check print-parse" in {
    import generators.{arbExpr, shrinkExpr}
    val normalizeCtx = new ExprTransformer {
      override def transformExprContext(ctx: ExprContext): ExprContext = Load
    }
    check {
      (expr0: Expr) =>
        val expr = normalizeCtx.transformExpr(expr0)
        val pretty = expr.pretty
        val parsed = try ExpressionParser.parse(pretty) catch {
          case NonFatal(err) =>
            System.err.println(s"Parse failure in:\n$pretty")
            throw err
        }
        if (parsed != expr) {
          System.err.println(s"$expr re-parsed to $parsed (via '$pretty')")
        }
        ExpressionParser.parse(expr.pretty) == expr
    }
  }

  // TODO: more stuff here

}
