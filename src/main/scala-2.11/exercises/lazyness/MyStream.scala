package exercises.lazyness

import MyStream._

/**
 * Created by steve on 9/7/2015.
 */
sealed trait MyStream[+A] {
  def headOptionOld: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    def fold(soFar: List[A], remaining: MyStream[A]): List[A] = remaining match {
      case Empty => soFar
      case Cons(h,t) => fold(h() :: soFar, t())
    }

    fold(Nil, this).reverse
  }

  def take(n: Int): MyStream[A] = this match {
    case Cons(h, _) if (n == 1) => cons(h(), empty[A])
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def take2(n: Int): MyStream[A] = {
    def generate(s: (Int, MyStream[A])): Option[(A, (Int, MyStream[A]))] = s._2 match {
      case Cons(h,t) if (s._1==1) => Some((h(),(0,empty[A])))
      case Cons(h,t) if (s._1>1) => Some((h(),(s._1-1,t())))
      case _ => None
    }
    MyStream.unfold((n,this))(generate)
  }

  def drop(n: Int): MyStream[A] = this match {
    case Cons(h, t) if n > 0 => t() drop n - 1
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, acc) => p(a) || acc)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def takeWhile(p: A => Boolean): MyStream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty[A])

  def takeWhile2(p: A => Boolean): MyStream[A] = {
    def generate(s: MyStream[A]): Option[(A,MyStream[A])] = s match {
      case Cons(h,t) if (p(h())) => Some((h(), t()))
      case _ => None
    }
    unfold(this)(generate)
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): MyStream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def map2[B](f: A => B): MyStream[B] = {
    def generate(s: MyStream[A]): Option[(B,MyStream[A])] = s match {
      case Empty => None
      case Cons(h,t) => Some((f(h()), t()))
    }
    unfold(this)(generate)
  }

  def filter(p: A => Boolean): MyStream[A] =
    foldRight(empty[A])((a,acc) => if (p(a)) cons(a,acc) else acc)

  def append[B >: A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((a,acc) => cons(a,acc))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])((a,acc) => f(a).append(acc))

  def zipWith[B,C](other: MyStream[B])(f: (A,B) => C): MyStream[C] = {
    def generate(s: (MyStream[A], MyStream[B])): Option[(C,(MyStream[A],MyStream[B]))] = s match {
      case (Cons(a,at),Cons(b,bt)) => Some((f(a(),b()),(at(),bt())))
      case _ => None
    }

    unfold((this,other))(generate)
  }

  def zipAll[B](other: MyStream[B]): MyStream[(Option[A],Option[B])] = {
    def generate(s: (MyStream[A],MyStream[B])): Option[((Option[A],Option[B]),(MyStream[A],MyStream[B]))] = s match {
      case (Empty,Empty) => None
      case (Cons(ah,at),Cons(bh,bt)) => Some(((Some(ah()),Some(bh())),(at(),bt())))
      case (Cons(ah,at),Empty) => Some(((Some(ah()),None),(at(),Empty)))
      case (Empty,Cons(bh,bt)) => Some(((None,Some(bh())),(Empty,bt())))
    }
    unfold((this, other))(generate)
  }

  def startsWith[B >: A](ss: MyStream[B]): Boolean = {
    zipAll(ss).takeWhile(_._2 != None).forAll(x => x._1 == x._2)
  }

  def tails: MyStream[MyStream[A]] = {
    def generator(s: MyStream[A]): Option[LazyPair[MyStream[A],MyStream[A]]] = s match {
      case Empty => None
      case Cons(_,t) => Some(LazyPair(()=>s,t))
    }
    lazyUnfold(this)(generator)
  }

  def hasSubSequence[B >: A](ss: MyStream[B]): Boolean = tails exists (_ startsWith ss)
}

case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

case class LazyPair[A,B](a: () => A, b: () => B)

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]) : MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A] : MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] = {
    if (as.isEmpty) empty[A] else cons[A](as.head, apply(as.tail: _*))
  }

  //def constant[A](c: A): MyStream[A] = cons(c, constant(c))

  //def from(n: Int): MyStream[Int] = cons(n, from(n+1))

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): MyStream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s2)) => MyStream.cons(a, unfold(s2)(f))
    }
  }

  def lazyUnfold[A,S](z: S)(f: S => Option[LazyPair[A,S]]): MyStream[A] = {
    def accumulate(acc: => MyStream[A], s: S): MyStream[A] = {
      f(s) match {
        case None => acc
        case Some(p) => accumulate(cons(p.a(), acc), p.b())
      }
    }

    f(z) match {
      case None => empty[A]
      case Some(p) => MyStream.cons(p.a(), lazyUnfold(p.b())(f))
    }
  }


  def constant[A](c: A): MyStream[A] = unfold(c)(a => Some((a,a)))

  def from(n: Int): MyStream[Int] = unfold(n)(x => Some((x,x+1)))
}