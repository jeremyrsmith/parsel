package parsel.quote

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import parsel.ast._
import parsel.quote.syntax._


class QuoteTest extends AnyFreeSpec with Matchers {

  "py" - {
    "passes for parseable programs" in {
      val ast =
        py"""
             def foo(bar: Thing = "baz"):
               return something
          """
      ast.shouldEqual(Module(Seq(FunctionDef(
        Name("foo"),
        Arguments(Nil, Seq(Arg(Name("bar"), Some(Name("Thing")), None)), None, Nil, Nil, None, Seq(Constant(StringLiteral("baz", None)))),
        Seq(Return(Some(Name("something")))),
        Nil,
        None,
        None))))
    }

    "fails for unparseable programs" in {
      assertDoesNotCompile(
        """ py"now it won't parse because this is nonsense" """
      )
    }

    "can splice values" in {
      val xxxxx = BigInt(10)
      val yyy = "hi I'm ted"
      val result = py"a = $xxxxx; b = $yyy"

      result shouldEqual Module(Seq(
        Assign(Seq(Name("a")), Constant(IntegerLiteral(xxxxx)), None),
        Assign(Seq(Name("b")), Constant(StringLiteral(yyy)), None)
      ))
    }
  }

  "pyq" - {
    "works when values can be quoted" in {
      val xxxxx = 10
      val yyy = "hi I'm ted"
      val quotedTree = pyq"a = $xxxxx; b = $yyy"
      val names = quotedTree.symbols.keys.toSeq.sortBy(_.name)
      val xName = names.head
      val yName = names(1)
      val tree = quotedTree.doQuote()
      tree shouldEqual Module(Seq(
        Assign(Seq(xName), Constant(IntegerLiteral(xxxxx)), None),
        Assign(Seq(yName), Constant(StringLiteral(yyy)), None),
        Assign(Seq(Name("a")), xName, None),
        Assign(Seq(Name("b")), yName, None)
      ))

      tree.pretty shouldEqual
        s"""${xName.pretty} = 10
           |${yName.pretty} = 'hi I\\'m ted'
           |a = ${xName.pretty}
           |b = ${yName.pretty}""".stripMargin

    }

    "can splice a tree" in {
      val tree1 = py"return foo"
      val tree2 =
        pyq"""
             def fn():
                 $tree1
        """

      tree2.tree shouldEqual Module(List(FunctionDef(Name("fn"), Arguments.empty, List(Return(Some(Name("foo")))), Nil, None, None)))
    }

    "can splice a sequence of trees" in {
      val trees = List(
        py"x = 1",
        py"return x"
      )
      val tree2 =
        pyq"""
             def fn():
                 $trees
        """

      tree2.tree shouldEqual Module(List(FunctionDef(Name("fn"), Arguments.empty, List(Assign(List(Name("x")), Constant(IntegerLiteral(1)), None), Return(Some(Name("x")))), Nil, None, None)))
    }

  }

  "pye" - {
    "parses an expression" in {
      val result = pye"x + 5"
      result shouldEqual BinOp(Name("x"), Add, Constant(IntegerLiteral(BigInt(5))))
    }

    "splices values" in {
      val value = BigInt(5)
      val result = pye"x + $value"
      result shouldEqual BinOp(Name("x"), Add, Constant(IntegerLiteral(value)))
    }

    "splices trees" in {
      val expr1 = pye"x + 5"
      val expr2 = pye"y * $expr1"
      expr2 shouldEqual BinOp(Name("y"), Mult, BinOp(Name("x"), Add, Constant(IntegerLiteral(BigInt(5)))))
    }

  }
}
