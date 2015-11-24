package algebra


object Polynomial {

  def apply[T <: Ring#RingElement](map: Map[Int, T]) = new Polynomial[T](map)


  def apply2(map: T1): T2 = {

    val normalMap: T1 = {
      val theMapList = map.toList
      def newMapList(oldMapList: List[(Int, field.T2)]): List[(Int, field.T2)] = oldMapList match {
        case Nil => Nil
        case ((x1, field.zero) :: xs) => newMapList(xs)
        case ((x1, x2) :: xs)  => (x1, x2) :: newMapList(xs)
      }
      val theNewMapList = newMapList(theMapList)
      val theNewMap = theNewMapList.toMap
      theNewMap
    }
    new Polynomial(normalMap)
  }

  def buildFromMap(map: field.polyRing.T1): T2 = {
    val polyOverFp = field.polyRing.builder(map).asInstanceOf[T1]
    new Polynomial(polyOverFp)
  }


}

class Polynomial[T <: Ring#RingElement](map1: Map[Int, T]) {

  private val mapList: List[(Int, T)] = map1.toList
  val ring = mapList.head._2.fatherRing
  val zero = ring.zero.asInstanceOf[T] // The zero of the ring



  private def loop(list: List[(Int, T)]): List[(Int, T)] = list match {
    case Nil => Nil
    case ((x1, x2) :: xs) if x2 == zero => loop(xs)
    case ((x1, x2) :: xs)  => (x1, x2) :: loop(xs)
  }

  private val goodList = loop(mapList)
  val map = goodList.toMap

  def add(other: Polynomial[T]) = {
    def addT(other: Polynomial[T], x: Int): T = {
      val a = this.map.getOrElse(x, zero).asInstanceOf[ring.T2]
      val b = other.map.getOrElse(x, zero).asInstanceOf[ring.T2]
      val c = a + b
      c.asInstanceOf[T]
    }
    val exponents = (map.keySet ++ other.map.keySet).toList
    def recursion(exp: List[Int]): Map[Int, T] = exp match {
      case Nil => Map[Int, T]()
      case x :: xs => recursion(xs) + (x -> addT(other, x))
    }
    val temporalMap = recursion(exponents)
    Polynomial(temporalMap)
  }

  val degree: Int = {
    val step1 = map.keySet
    if (this.map == Map(0 -> ring.zero) || this.map == Map[Int, ring.T2]()) -999999 else step1.max
  }

  val lc: T = {
    if (degree == -999999) zero else this.map(degree)
  }


  def isMonic: Boolean = this.lc == ring.one

  def +(other: Polynomial[T]) = this.add(other)

  def multiply(other: Polynomial[T]) = {

    val step1 = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2.asInstanceOf[ring.T2] * j._2.asInstanceOf[ring.T2])
    val exponents = step1.map(x => x._1).distinct
    val step2 = for (i <- exponents) yield step1.filter(x => x._1 == i)

    def sumListInRing(list1: List[ring.T2]): ring.T2 = list1 match {
      case Nil => ring.zero
      case x :: xs => x + sumListInRing(xs)
    }

    def sumCoef(l: List[(Int, ring.T2)]): (Int, ring.T2) = {
      (l.head._1, sumListInRing(l.map(x => x._2)))
    }

    val step3 = step2.map(sumCoef)
    val step4 = step3.toMap
    Polynomial(step4)

  }

  def *(other: Polynomial[T]) = this.add(other)


  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Polynomial[T]]
    if (that == null) false
    else {
      val a1: Boolean = this.map == that.map
      val a2: Boolean = (this.map == Map[Int, ring.T2]()) && (that.map == Map(0 -> ring.zero))
      val a3: Boolean = (that.map == Map[Int, ring.T2]()) && (this.map == Map(0 -> ring.zero))
      a1 || a2 || a3
    }
  }









}

