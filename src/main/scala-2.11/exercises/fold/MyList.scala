package exercises.fold

/**
 * Created by steve on 9/6/2015.
 */
sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

  def foldLeft[A,B](as: MyList[A], z: B)(f: (A,B) => B) : B = {
    as match {
      case MyNil => z
      case Cons(a, t) => foldLeft(t, f(a, z))(f)
    }
  }

  def reverse[A](as: MyList[A]) : MyList[A] = {
    foldLeft(as, MyNil : MyList[A])(Cons(_,_))
  }

  def foldRight[A,B](as: MyList[A], z: B)(f: (A,B) => B) : B = {
    foldLeft(reverse(as),z)(f)
  }

  def append[A](as: MyList[A], bs: MyList[A]) : MyList[A] = {
    foldRight(as, bs)(Cons(_,_))
  }

  def flatten[A](ass: MyList[MyList[A]]) : MyList[A] = {
    foldRight(ass, MyNil: MyList[A])(append(_,_))
  }

  def map[A,B](as: MyList[A])(f: A => B) : MyList[B] = {
    foldRight(as, MyNil: MyList[B])((a: A, bs: MyList[B]) => Cons(f(a),bs))
  }

  def flatmap[A,B](as: MyList[A])(f: A => MyList[B]) : MyList[B] = {
    foldRight(as, MyNil : MyList[B])((a: A, bs: MyList[B]) => append(f(a),bs))
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    def ListIf(a: A) = if (f(a)) MyList(a) else MyNil
    flatmap(as)(ListIf)
  }

  def zipWith[A,B,C](as: MyList[A], bs: MyList[B])(f: (A,B) => C) : MyList[C] = {
    def accumulate(as: MyList[A], bs: MyList[B], acc: MyList[C]) : MyList[C] = {
      (as,bs) match {
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (Cons(a,at),Cons(b,bt)) => accumulate(at, bt, Cons(f(a,b),acc))
      }
    }

    accumulate(as, bs, MyNil)
  }

  def hasSubSequence[A](as: MyList[A], subs: MyList[A]) : Boolean = {
    def beginsWith(rs: MyList[A], ss: MyList[A]) : Boolean = {
      rs match {
        case MyNil => (ss == MyNil)
        case Cons(r, ts) => ss match {
          case MyNil => true
          case Cons(s, sts) => if (s != r) false else beginsWith(ts, sts)
        }
      }
    }

    subs match {
      case MyNil => true
      case _ => if (beginsWith(as,subs)) true else as match {
        case MyNil => false
        case Cons(a,at) => hasSubSequence(at, subs)
      }
    }
  }
}