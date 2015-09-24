package exercises.monoid

import exercises.parallel.Par
import exercises.parallel.Par._

/**
 * Created by steve on 9/19/2015.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  }
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ::: a2

    def zero = Nil
  }
  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 match {
      case None => a2
      case _ => a1
    }

    def zero = None
  }
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A) = a1 compose a2

    def zero = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A) = m.op(a2,a1)

    def zero = m.zero
  }

  def endoMonoid2[A] = dual(endoMonoid[A])

  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = new Monoid[(A => B)]{
    def op(a1: (A) => B, a2: (A) => B) = (a => b.op(a1(a),a2(a)))

    def zero = (a => b.zero)
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b,a)=>m.op(b,f(a)))

  def myFoldRight[A,B](as: List[A])(z: B)(f: (A,B) => B) = {
    def curriedFn(a: A)(b: B) = f(a,b)
    def partialFn(a: A): B => B = curriedFn(a)

    foldMap(as,endoMonoid[B])(partialFn)(z)
  }

  def myFoldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B) = {
    def curriedFn(a: A)(b: B) = f(b,a)
    def partialFn(a: A): B => B = curriedFn(a)

    foldMap(as,endoMonoid2[B])(partialFn)(z)
  }

  def foldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
    case 0 => m.zero
    case 1 => f(as(0))
    case _ => (as splitAt as.length/2) match {
      case (as1,as2) => m.op(foldMap(as1, m)(f),foldMap(as2, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]) = map2(a1,a2)((a,b)=>m.op(a,b))

    def zero = unit(m.zero)
  }

  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM = par(m)
    def fPar(a: A) = unit(f(a))
    def subResult(ss: IndexedSeq[A]) = if (ss.length>1000) fork(parFoldMap(ss,m)(f)) else parFoldMap(ss,m)(f)

    as.length match {
      case 0 => unit(m.zero)
      case 1 => fPar(as(0))
      case _ => (as splitAt as.length / 2) match {
        case (as1, as2) => parM.op(subResult(as1), subResult(as2))
      }
    }
  }

  def parFoldMap2[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val inter = Par.parMap(v.toList)(f)
    flatMap(inter) { (bs: List[B]) =>
      foldMap(bs, par(m))(b => Par.lazyUnit(b))
    }
  }
}