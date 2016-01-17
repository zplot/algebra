import Hopf._
import algebra._
import  Utils._
import scala.language.implicitConversions




object TestTree extends App {

  println("Empezamos Rooted Trees")

  // Hace falta importar este implicit para que funcione val test1 = "**".weight
  // Aunque no es necesario importar el implicit string2List para que funcione val test2 = Tree("**")
  // No puedo entender porqué: misterio
  import Tree.string2Tree



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
  val test1 = "**".weight
  val test2 = Tree("**")



}




