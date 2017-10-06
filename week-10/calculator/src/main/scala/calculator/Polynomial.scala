package calculator

object Polynomial {


  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal[Double] {
      (b()*b()) - (4* a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]] {
      val x = math.sqrt(delta())
      val s1 = ((-b()) + x)/ (2*a())
      val s2 = ((-b()) - x)/ (2*a())
      Set(s1,s2)
    }

  }


}
