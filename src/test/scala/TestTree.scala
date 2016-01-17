import botany._
import algebra._
import  Utils._
import scala.language.implicitConversions




object TestTree extends App {

  println("Empezamos Rooted Trees")


  import Tree.string2Tree
  implicit def string2List(s: String): List[Tree] = string2Tree(s).children



  val t5 = Tree(List())
  println(t5)
  val t6 = Tree(List(t5, t5, t5))
  println(t6)
  val t7 = Tree(List(t5, t6, t5))
  println("t7 = " + t7)
  println(t7.weight)
  val t8 = Tree.string2Tree(t7.toString)
  println("t8 = " + t8)
  val t9 = "*"
  val test1 = "**^".weight
  println(test1)
  val test2 = Tree("***^^")
  println(test2.weight)

  val z = t7.canonicalForm
  println(z)

  val zx = Tree.orderTree(Tree("**^**^*^^"))
  println(zx)



}




