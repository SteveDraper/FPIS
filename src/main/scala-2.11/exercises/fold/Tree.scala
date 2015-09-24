package exercises.fold

/**
 * Created by steve on 9/6/2015.
 */
sealed trait Tree[+A] {
  def size: Int
  def depth: Int
  def map[B](f: A => B) : Tree[B]
}

object None extends Tree[Nothing] {
  val size = 0
  val depth = 0
  def map[B](f: Nothing => B) = None
}

case class Leaf[A](value: A) extends Tree[A]
{
  val size = 1
  val depth = 0
  def map[B](f: A => B) = Leaf[B](f(value))
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
{
  def size = left.size + right.size + 1
  def depth = 1 + Math.max(left.depth,right.depth)
  def map[B](f: A => B) = {
    Branch(left.map(f),right.map(f))
  }
}

object Tree {
  def apply[A](a: A) : Tree[A] = new Leaf(a)
  def apply[A](left: Tree[A], right: Tree[A]) : Tree[A] = new Branch(left,right)

  def max[A](at: Tree[A])(fmax: (A,A) => A) : A = {
    at match {
      case None => throw new Error("max of empty tree")
      case Leaf(a) => a
      case Branch(left,right) => fmax(max(left)(fmax), max(right)(fmax))
    }
  }

  def fold[A,B](at: Tree[A], Zero: B)(f: (A,B) => B)(g: (B,B) => B) : B = {
    at match {
      case None => Zero
      case Leaf(a) => f(a, Zero)
      case Branch(left,right) => g(fold(left, Zero)(f)(g),fold(right, Zero)(f)(g))
    }
  }
}