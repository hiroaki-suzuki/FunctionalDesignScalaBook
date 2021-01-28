package rirazou
package chapter02

import scala.annotation.tailrec

object GettingStarted {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def main(args: Array[String]): Unit = {
    factorial(10)
  }
}
