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
  }

  "pyq" - {
    "works when values can be quoted" in {
      val xxxxx = 10
      val yyy = "hi I'm ted"
      val quotedTree = pyq"a = $xxxxx; b = $yyy"
      val names = quotedTree.symbols.keys.toSeq.sortBy(_.name)
      val xName = names.head
      val yName = names(1)
      val tree = quotedTree.doQuoted()
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
  }
}
