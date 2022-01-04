package parsel.parse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import parsel.ast._

import java.io.File

class ParserTest extends AnyFreeSpec with Matchers with Checkers {

  "assignments" in {
    Parser.parse(
      """x = 10
        |x,y = 10
        |""".stripMargin
    ) shouldEqual Module(Seq(
      Assign(Seq(Name("x")), Constant(IntegerLiteral(BigInt(10))), None),
      Assign(Seq(ConstructTuple(Seq(Name("x"), Name("y")), Store)), Constant(IntegerLiteral(BigInt(10))), None)))
  }

  Option(getClass.getClassLoader.getResource("test1.py"))
    .filter(_.getProtocol == "file")
    .map(_.getPath)
    .map(new File(_))
    .map(_.getParentFile)
    .map(resourceDir => new File(resourceDir, "testfiles"))
    .filter(_.isDirectory)
    .orElse(Option(new File("src/test/resources/testfiles")).filter(_.isDirectory))
    .map(_.listFiles())
    .map{ fs => fs.foreach(println); fs }
    .map(_.filter(_.getName.endsWith(".py"))) match {
    case None => "parser is idempotent (skipping, couldn't read testfiles directory)" ignore {}
    case Some(testFiles) =>
      "parser is idempotent" - {
        // this test parses each of the .py files in resources/testfiles, and then pretty-prints the AST, and
        // then re-parses the pretty-printing. This gives an indication that either:
        //   A. The parser works correctly, or
        //   B. The parser and pretty-printer are broken in the same way.
        // The reason for doing this is that it would be really cumbersome to parse things and then verify that their
        // AST is correct. It's really annoying to match against expected ASTs, because constructing them is a lot of
        // code. In general, case B above is pretty unlikely (but certainly not impossible!) and I consider that good
        // enough (given enough sample files).
        def testFile(file: File): Unit = {
          file.getName in {
            val input = scala.io.Source.fromFile(file, "utf-8") match {
              case src => try src.mkString finally src.close()
            }
            val parsed1 = Parser.parse(input)
            val formatted = parsed1.pretty
            val parsed2 = Parser.parse(formatted)
            formatted shouldEqual parsed2.pretty
            parsed1 shouldEqual parsed2
          }
        }

        def negFile(file: File): Unit = {
          file.getName in {
            val input = scala.io.Source.fromFile(file, "utf-8") match {
              case src => try src.mkString finally src.close()
            }
            a [ParseError] should be thrownBy {
              Parser.parse(input)
            }
          }
        }

        testFiles.foreach {
          case file if file.getName.startsWith("neg_") => negFile(file)
          case file => testFile(file)
        }
      }
  }

//  "check generated trees" in {
//    import org.scalacheck.ScalacheckShapeless._
//    check {
//      (module: Module) =>
//        val printed = module.pretty
//        val parsed = Parser.parse(printed)
//        parsed == module
//    }
//  }

}
