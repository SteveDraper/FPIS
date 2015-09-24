package exercises.monoid

import exercises.fold.{Branch, Leaf, Tree, None}

/**
 * Created by steve on 9/21/2015.
 */
trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldRight(as)(Nil:List[A])((h,t)=>h::t)
}

class FoldableTree[+A] extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = {
    def treeFold(t: Tree[A], acc: B): B = t match {
      case None => acc
      case Leaf(a) => f(a, acc)
      case Branch(l,r) => treeFold(l,treeFold(r, acc))
    }

    treeFold(as,z)
  }

  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =  {
    def treeFold(acc: B, t: Tree[A]): B = t match {
      case None => acc
      case Leaf(a) => f(acc, a)
      case Branch(l,r) => treeFold(treeFold(acc,l),r)
    }

    treeFold(z, as)
  }

  def foldMap[A, B](as: Tree[A])(f: (A) => B)(implicit mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Leaf(a) => f(a)
    case Branch(l,r) => mb.op(foldMap(l)(f),foldMap(r)(f))
  }
}

class FoldableOption[+A] extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case Some(a) => f(a,z)
    case _ => z
  }

  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case Some(a) => f(z,a)
    case _ => z
  }

  def foldMap[A, B](as: Option[A])(f: (A) => B)(implicit mb: Monoid[B]) =  as match {
    case Some(a) => f(a)
    case _ => mb.zero
  }
}