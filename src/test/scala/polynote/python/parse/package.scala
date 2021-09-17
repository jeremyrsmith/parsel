package polynote.python

package object parse {
  val bigFile: String = scala.io.Source.fromURI(getClass.getClassLoader.getResource("test1.py").toURI) match {
    case src => try src.mkString finally src.close()
  }
}
