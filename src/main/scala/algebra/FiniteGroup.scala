package algebra

trait FiniteGroup extends Group {

  type T1
  type T2 <: FiniteGroupElement

  val structureId: String


  def builder(x: T1): T2

  val elements: Set[T2]

  def elementsOrdered: List[T2] = one :: (elements - one).toList

  def cardinal: Int = elements.size

  def isAbelian: Boolean = {
    val producto =
      for (i <- elements; j <- elements) yield i.multiply(j) == j.multiply(i)
    !producto.contains(false)
  }

  def cayleyTable(): Unit = {
    val list1 = (1 to cardinal).toList // Enumeración de los elementos
    val traductor = (list1 zip elementsOrdered).toMap
    val traductorInverso = (list1 zip elementsOrdered).toMap.map(_.swap)
    println()
    println("Cayley table of " + this.structureId + ":")
    println()
    for (i <- 1 to cardinal) {
      for (j <- 1 to cardinal) {
        print(traductorInverso(traductor(i).multiply(traductor(j))) + " ")
      }
      println()
    }
    println()
  }




  def cayleyTableOK: List[List[String]] = {

    val list1 = (1 to cardinal).toList // Enumeración de los elementos
    val traductor = (list1 zip elementsOrdered).toMap
    val traductorInverso = (list1 zip elementsOrdered).toMap.map(_.swap)

    val tmp1 = List.range( 1, cardinal + 1)

    def row(i: Int): List[Int] = {
      val tmp1 = for (j <- 1 to cardinal) yield traductorInverso(traductor(i).multiply(traductor(j)))
      val tmp2 = tmp1.toList
      tmp2
    }



    val tmp2: List[List[Int]] = tmp1.map( i => row(i))
    val tmp3: List[List[String]] = tmp2.map(row => row.map(elem => elem.toString))
    tmp3

  }




  trait FiniteGroupElement extends GroupElement {

    val fatherFiniteGroup = FiniteGroup.this


  }
}
