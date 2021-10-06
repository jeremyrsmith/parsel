package parsel

import parsel.ast.Name

import java.util.concurrent.atomic.AtomicInteger

package object quote {

  private val nextName: AtomicInteger = new AtomicInteger(0)

  def freshName: Name = Name(s"_parsel${nextName.getAndIncrement()}")

}
