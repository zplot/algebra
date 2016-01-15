import algebra._
import  Utils._




object TestRootedTree extends App {

  println("Empezamos Rooted Trees")



  val t5 = Tree(List())
  println(t5)
  val t6 = Tree(List(t5, t5, t5))
  println(t6)
  val t7 = Tree(List(t5, t6, t5))
  println(t7)
  println(t7.weight)



}