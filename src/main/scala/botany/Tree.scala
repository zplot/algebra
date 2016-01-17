package botany

import scala.language.implicitConversions


/*
TODO CanonicalForm
1. Asignar un identificador a cada nodo
2. Asignar un peso a cada nodo
3. Reordenar nodos por pesos
4. Devolver árbl ordenado
*/





// http://aperiodic.net/phil/scala/s-99/
object Tree {

  implicit def string2Tree(s: String): Tree = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp = splitChildStrings(1).map(string2Tree(_))
    Tree(tmp)
  }

}



case class Tree(children: List[Tree]) {

  def weight: Int = children.foldLeft(1)(_ + _.weight)

  def canonicalForm: Tree = ???

  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree]
    if (that == null) false
    else canonicalForm == that.canonicalForm
  }

}

// Here's the technique to have a private constructor and a public apply method.
// http://stackoverflow.com/questions/20030826/scala-case-class-private-constructor-but-public-apply-method
trait Tree2 {
  val children: List[Tree2]
}

object Tree2 {

  val idsList: List[Int] = List[Int](0)

  def apply(children: List[Tree2]): Tree2 = {
    val last = idsList.head
    val next = last + 1
    next :: idsList
    RichTree2(children)
  }


  private case class RichTree2(children: List[RichTree2]) extends Tree2 {

    val id: Int = idsList.head
    def weight: Int = children.foldLeft(1)(_ + _.weight)

  }
}








