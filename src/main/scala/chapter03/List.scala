package rirazou
package chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil        => sys.error("list is nil")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil        => sys.error("list is nil")
      case Cons(_, t) => Cons(h, t)
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

//  @tailrec
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
//    l match {
//      case Cons(h, t) if f(h) => dropWhile(t, f)
//      case _                  => l
//    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil          => sys.error("list is nil")
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }

  @tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _                  => as
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil        => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // case Cons(h, t) => Cons(h, append(t, a2))
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append2)
}
