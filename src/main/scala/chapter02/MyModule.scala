package rirazou
package chapter02

import scala.annotation.tailrec

// コメント!
/* 別のコメント */
/** 説明コメント */
object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def sum(v1: Int, v2: Int) = v1 + v2

  def concat(str1: String, str2: String): String = str1 + " " + str2

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: (A, B) => C): B => (A => C) = (b: B) => (a: A) => f(a, b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

    println(findFirst(Array("aaa", "bbb", "ccc"), (value: String) => value == "ccc"))
    println(findFirst(Array(1, 2, 3, 4, 5), (value: Int) => value == 4))
    println(isSorted(Array(1, 2, 3, 4, 5, 6), (v1: Int, v2: Int) => v1 > v2))
    println(isSorted(Array(1, 2, 3, 4, 6, 5), (v1: Int, v2: Int) => v1 > v2))

    println(sum(3, 5))
    val sum3 = partial1(3, sum)
    println(sum3(5))

    val concatCurry = curry(concat)("Fizz")
    println(concatCurry("Buzz"))

    val concatUncurry = uncurry(concat)("Buzz")
    println(concatUncurry("Fizz"))

    val nameHiroaki = compose((b: String) => b + "Suzuki", (a: String) => a + " ")
    println(nameHiroaki("Hiroaki"))
    println(nameHiroaki("Moe"))
  }
}
