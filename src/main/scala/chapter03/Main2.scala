package rirazou
package chapter03

object Main2 extends App {
  val tree = Branch(Branch(Leaf("A"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))

  println(Tree.size(tree))
  println(Tree.size2(tree))

  val iTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(5), Leaf(3)))
  println(Tree.maximum(iTree))
  println(Tree.maximum2(iTree))

  println(Tree.depth(iTree))
  println(Tree.depth2(iTree))

  println(Tree.map(tree)(_ + " Group"))
  println(Tree.map2(tree)(_ + " Group"))
}
