package rirazou
package chapter03

object TreeApp extends App {
  val tree = Branch(Branch(Leaf("A"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))

  println(Tree.size(tree))
}
