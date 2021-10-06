package parsel.quote

import parsel.ast.Tree

trait Quotable[-T] {
  def quote(value: T): Tree
}

case class QuotedTree(

)

case class QuotedValue[A](
  value: A,

  quotable: Quotable[A]
)