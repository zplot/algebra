package botany

import scala.language.implicitConversions
import scala.util.parsing.input.Position


/*
TODO CanonicalForm
1. Asignar un identificador a cada nodo
2. Asignar un peso a cada nodo
3. Reordenar nodos por pesos
4. Devolver árbl ordenado
*/


case class Point(x: Int, y: Int)
case class Node(position: Point)
case class Edge(pos1: Point, pos2: Point)
case class State(cursorActualPos: Point, cursorPreviousPos:Point, nodes: List[Node], edges: List[Edge])



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



  // Eats a string and drops a list of nodes and a list of edges
  def string2SVG1(s: String): (List[Node], List[Edge]) = {

    val hist = List[State](State(Point(0, 1), Point(0, 1), List[Node](), List[Edge]()))

    def stringAnalyze(s: List[Char], hist: List[State]): (List[Char], List[State]) = s match {

      case Nil => (Nil, hist)
      case '*' :: xs => stringAnalyze(xs, newNode(hist))
      case '^' :: xs => stringAnalyze(xs, goUp(hist))

    }

    def newNode(hist: List[State]): List[State] = {

      val actualState = hist.head
      val actual = actualState.cursorActualPos
      val x = actual.x
      val y = actual.y
      val previous = actualState.cursorPreviousPos
      val nodes = actualState.nodes
      val edges = actualState.edges
      val newY = actual.y - 1

      def firstEmptyX(p: Point): Int = {
        val tmp1 = nodes.filter(node => node.position.y == newY) // Nodes at y = newY
        val tmp2 = tmp1.map(node => node.position.x) // List of xs
        val tmp3 = if (tmp2.isEmpty) 0 else tmp2.max + 1 // highest x
        tmp3 // next empty x
      }

      val newX = firstEmptyX(actual)
      val newNode = Node(Point(newX, newY))

      val newEdge = Edge(actual, newNode.position)

      val newState = State(Point(newX, newY), Point(x, y), newNode :: nodes, newEdge :: edges)
      newState :: hist

    }

    def goUp(hist: List[State]): List[State] = {

      val actualState = hist.head
      val actual = actualState.cursorActualPos
      val previous = actualState.cursorPreviousPos
      val nodes = actualState.nodes
      val edges = actualState.edges
      val newPreviousPosition = actual


      val newState = State(previous, newPreviousPosition, nodes, edges)
      newState :: hist

    }

    val tmp = stringAnalyze(s.toList, hist)

    (tmp._2.head.nodes, tmp._2.head.edges)



  }
















  def orderTree(t: Tree): Tree = {

    if (t.children != List())  {
      val tmp3 = t.children.map( x => orderTree(x))
      val tmp4 = Tree(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Tree(tmp5)
      tmp6
    } else t

  }

}









case class Tree(children: List[Tree]) {

  def weight: Int = children.foldLeft(1)(_ + _.weight)
  def canonicalForm = Tree.orderTree(this)

  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree]
    if (that == null) false
    else Tree.orderTree(this).children == Tree.orderTree(that).children
  }

}









