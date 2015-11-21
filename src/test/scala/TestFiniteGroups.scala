import algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = DirectProduct(S(2), S(3))


  grupo.cayleyTable()
  println(grupo.cayleyTable2)


}