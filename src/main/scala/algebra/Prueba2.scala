package algebra


object PolyOverRing {

  def apply(ring: Ring): PolyOverRing = {
    new PolyOverRing(ring)
  }
}

class PolyOverRing private(val ring: Ring) extends Ring {

  type T1 = Map[Int, ring.T2]
  type T2 = Polynomial

  object Polynomial {

    def apply(map: T1): T2 = {

      new Polynomial(map)
    }


  }

  class Polynomial private(val map: T1) extends RingElement {

  }

}


