package algebra

object Polynomial {

/*  def apply[T <: Ring](map: Map[Int, T]): Polynomial[T] = {

    val normalMap = ???

    new Polynomial[T](normalMap)
  }
}*/

trait Polynomial[T <: Ring#RingElement]  {




  def add(other: Polynomial[T]) = ???

  def minus(other: Polynomial[T]) = ???

  def multiply(other: Polynomial[T]) = ???

  def multiply(other: T): Polynomial[T] = ???

  val degree: Int = ???

  val lc: T = ???



  def *(other: Polynomial[T]) = this.multiply(other)
  def *(other: T) = this.multiply(other)
  def +(other: Polynomial[T]) = this.add(other)
  def -(other: Polynomial[T]) = this.minus(other)


  override def toString = ???


  override def equals(other: Any): Boolean = ???



}


