package parsel.quote

import parsel.ast._

trait Quotable[-T] {
  def quote(value: T): Expr
}

object Quotable {
  def instance[T](fn: T => Expr): Quotable[T] = new Quotable[T] {
    override def quote(value: T): Expr = fn(value)
  }

  implicit val int: Quotable[Int] = instance(i => Constant(IntegerLiteral(BigInt(i))))
  implicit val long: Quotable[Long] = instance(l => Constant(IntegerLiteral(BigInt(l))))
  implicit val bigInt: Quotable[BigInt] = instance(bi => Constant(IntegerLiteral(bi)))
  implicit val float: Quotable[Float] = instance(f => Constant(FloatLiteral(BigDecimal(f))))
  implicit val double: Quotable[Double] = instance(d => Constant(FloatLiteral(BigDecimal(d))))
  implicit val bigDec: Quotable[BigDecimal] = instance(bd => Constant(FloatLiteral(bd)))
  implicit val string: Quotable[String] = instance(s => Constant(StringLiteral(s)))
  implicit val bool: Quotable[Boolean] = instance(b => Constant(BooleanLiteral(b)))
}

case class QuotedTree(
  tree: CompleteTree,
  symbols: Map[Name, QuotedValue[_]]
) {
  def doQuoted(): CompleteTree = {
    val transformer = new TreeTransformer {
      override def transformModule(module: Module): Module = super.transformModule(module) match {
        case Module(stats) =>
          // insert name assignments
          // TODO: it would be better if this was only done for stable splices that are used more than once.
          val assignments = symbols.values.toList.map {
            qv => Assign(Seq(qv.name), qv.doQuote(), None)
          }
          Module(assignments ++ stats)
      }
      override def transformExpr(expr: Expr): Expr = expr match {
        case name: Name if symbols.contains(name) => symbols(name).name
        case expr => super.transformExpr(expr)
      }
    }
    transformer.transformComplete(tree)
  }
}

case class QuotedValue[A](
  value: A,
  name: Name,
  quotable: Quotable[A]
) {
  def doQuote(): Expr = quotable.quote(value)
}