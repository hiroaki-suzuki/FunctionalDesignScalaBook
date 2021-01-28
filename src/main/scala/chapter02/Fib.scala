package rirazou
package chapter02

import scala.annotation.tailrec

object Fib extends App {
  def fib(n: Int): Int = {
    def loop(n: Int): Int = {
      if (n == 0 || n == 1) n
      else loop(n - 1) + loop(n - 2)
    }

    loop(n)
  }

  def fibAnswer(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  for (n <- 0 to 20) {
    println(fib(n))
  }
  println("------------------")
  for (n2 <- 0 to 20) {
    println(fibAnswer(n2))
  }
}
