
object Prueba extends App {


  object TipoA {
    def apply(z: Int): TipoA = {
      TipoA(2 * z)
    }
  }

  abstract case class TipoA private(z: Int)

  class TipoB(val w: Int) extends TipoA {




  }

  val a = new TipoB(3)
  println(a)

}