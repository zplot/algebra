package algebra

object Polynomial {

  def apply[T <: Ring](map: Map[Int, T]): Polynomial[T] = {

    val normalMap = ???

    new Polynomial[T](normalMap)
  }
}

class Polynomial[T <: Ring] (val map: Map[Int, T]) {


  def negate = this.multiply(ring.one.negate)

  def add(other: Polynomial[T]) = ???

  def minus(other: Polynomial[T]) = ???

  def multiply(other: Polynomial[T]) = ???

  val degree: Int = ???

  val lc: T = ???

  def multiply(other: Polynomial[T]): Polynomial[T] = ???


  def *(other: Polynomial[T]) = this.multiply(other)

  def isMonic: Boolean = this.lc == ring.one

  override def toString = ???


  override def equals(other: Any): Boolean = ???



}


