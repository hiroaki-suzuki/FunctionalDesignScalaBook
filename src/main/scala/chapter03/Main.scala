package rirazou
package chapter03

object Main extends App {
  val ints = List(1, 2, 3, 4)
  println(List.sum(ints))

  val ds = List(1.0, 2.0)
  println(List.product(ds))

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
  println(ex1)
  println(ex2)
  println(ex3)
  println(List.x)

  val list = List(1, 2, 3, 4, 5)
  val list1 = List.tail(list)
  println(list)
  println(list1)

  val list2 = List.setHead(list, 10)
  println(list2)

  val list3 = List.drop(list, 3)
  println(list3)

//  val list4 = List.dropWhile(list, (n: Int) => n <= 3)
//  println(list4)

  val list5 = List.append(list, List(10, 11, 12))
  println(list5)

  val list6 = List.init(list)
  println(list6)

  val xs: List[Int] = List(1, 2, 3, 4, 5)
//  val ex4 = List.dropWhile(xs, (x: Int) => x < 4)
  val ex5 = List.dropWhile(xs)(x => x < 4)

  val len = List.length(list)
  println(len)

  println(List.sum3(List(1, 2, 3, 4, 5)))
  println(List.product3(List(1.0, 2.0, 3.0)))
  println(List.length2(List(1, 2, 3, 4)))

  println(List.reverse(List(1, 2, 3, 4)))

  println(List.append2(List(1, 2, 3), List(4, 5, 6)))

  println(List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

  println(List.add1(List(1, 2, 3)))

  println(List.toStr(List(1.0, 1.5, 2.2)))

  println(List.map(List(1, 2, 3))(_ + 1))
  println(List.map(List("Hiroaki", "Moe", "Haruto", "Tomohiro"))(_ + " Suzuki"))
  println(List.filter(List(1, 2, 3, 4, 5))(_ > 3))

  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
  println(List.filter2(List(1, 2, 3, 4, 5))(_ > 3))

  println(List.plusList(List(1, 2, 3), List(4, 5, 6)))

  println(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
  println(
    List.zipWith(List("Hiro", "Moe"), List("Suzuki", "Kawasaki"))(_ + " " + _)
  )

  println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
}
