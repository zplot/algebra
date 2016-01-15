package algebra

import scala.language.implicitConversions

// http://aperiodic.net/phil/scala/s-99/



object Tree {

}

case class Tree(children: List[Tree]) {

  def weight: Int = children.foldLeft(1)(_ + _.weight)

  def canonicalForm: Tree = ???

  override def toString = " * " + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree]
    if (that == null) false
    else canonicalForm == that.canonicalForm
  }

}










