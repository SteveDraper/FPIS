import java.util.concurrent.ForkJoinPool

import exercises.monoid.{WC, Monoid, FoldableTree}
import Monoid._
import WC.countWords
import exercises.fold.{Tree, Leaf, Branch, None=>NoTree}

val l = List(1,2,3)

foldMap(l, intAddition)(x => x*2)
l.foldLeft(0)((x,y) =>x+y*2)
myFoldRight(l)(0)((x,y) =>x*2+y)
myFoldLeft(l)(0)((x,y) =>x+y*2)

val ls = List(-5,0,0,1,2,3,3,4,5)

ls.sum
/*foldMap(ls, stringMonoid)(_.toUpperCase())
ls.foldLeft("")((acc,s) => acc + s.toUpperCase())
myFoldRight(ls)("")((s,acc) => s.toUpperCase()+acc)
myFoldLeft(ls)("")((acc,s) => acc + s.toUpperCase())
foldMap(ls.toIndexedSeq, stringMonoid)(_.toUpperCase())*/

val orderedMonoid = new Monoid[Option[IndexedSeq[Int]]] {
  def op(a1: Option[IndexedSeq[Int]], a2: Option[IndexedSeq[Int]]) = (a1,a2) match {
    case (Some(IndexedSeq()),_) => a2
    case (_,Some(IndexedSeq())) => a1
    case (Some(l1@(ll:+lt)),Some(h2+:empty)) if (lt <= h2) => Some(l1:+h2)
    case _ => None
  }

  def zero = Some(IndexedSeq[Int]())
}
foldMap(ls, orderedMonoid)((x:Int)=>Some(IndexedSeq(x)))

countWords("the quick brown fox jumped over the lazy thingie")

val myTree = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),NoTree)).map(_.toString)

val treeFolder = new FoldableTree[Int]()

treeFolder.foldLeft(myTree)(stringMonoid.zero)(stringMonoid.op)

treeFolder.foldRight(myTree)(stringMonoid.zero)(stringMonoid.op)

treeFolder.foldMap(myTree)(identity)(stringMonoid)

treeFolder.toList(myTree)