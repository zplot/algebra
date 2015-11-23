package algebra


trait EuclideanDomain extends UFD {

  type T1
  type T2 <: EDElement


  def builder(x: T1): T2

  def normalPart(a: T2): T2

  def euclideanNorm(x: T2): Int

  def euclideanQ(a: T2, b: T2): T2

  def euclideanR(a: T2, b: T2): T2

  def euclideanDivision(a: T2, b: T2): (T2, T2) =
    (euclideanQ(a, b), euclideanR(a,b))

  def gcd(a: T2, b: T2): T2 =
    if (a == zero && b == zero) zero else {
      if (b == zero) a else gcd(b, euclideanR(a, b))
    }

  def bezout(a: T2, b: T2): (T2, T2)

  trait EDElement extends UFDElement {

  }
}
