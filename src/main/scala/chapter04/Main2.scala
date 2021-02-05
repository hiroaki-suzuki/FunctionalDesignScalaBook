package rirazou
package chapter04

object Main2 extends App {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  println(mean(Seq(1, 2, 3, 4, 5)))
  println(mean(Seq.empty))
}
