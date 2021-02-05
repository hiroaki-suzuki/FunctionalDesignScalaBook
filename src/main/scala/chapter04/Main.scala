package rirazou
package chapter04

object Main extends App {

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 }
  }

//  failingFn(1)

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    } catch { case e: Exception => 43 }
  }

  println(failingFn2(2))

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  println(mean(Seq(1, 2, 3, 4, 5)))

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  println(mean_1(IndexedSeq.empty, 0.0))
}
