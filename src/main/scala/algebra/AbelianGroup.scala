package algebra

trait AbelianGroup {

  type T1
  type T2 <: AbelianGroupElement

  def builder(x: T1): T2

  val structureId: String
  val zero: T2

  override def toString: String = structureId

  trait AbelianGroupElement {

    val fatherAbelianGroup = AbelianGroup.this

    def negate: T2
    def add(other: T2): T2
    def +(other: T2): T2 = this.add(other)
    def minus(other: T2): T2
    def -(other: T2): T2 = this.minus(other)


    def isZero: Boolean = this == zero

    override def toString: String
  }
}

