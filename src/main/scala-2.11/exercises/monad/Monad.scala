package exercises.monad

import exercises.parallel.Par
import exercises.parallel.Par.Par
import exercises.parsers.Parsers
import exercises.stateful.State

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B) : F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: (A) => B) = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)

  def replicateN[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => F[Boolean]):F[List[A]] =
    ms.map(a=>map(f(a))(if(_) List(a) else Nil )).foldRight(unit(Nil:List[A]))(map2(_,_)((x:List[A],y:List[A])=>x:::y))

  def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM2(t)(f)
        else map(filterM2(t)(f))(h :: _))
    }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => {
    flatMap(f(a))(g)
  }

  def flatMap2[A,B](fa: F[A])(f: A => F[B]): F[B] = {
    compose((_:Unit)=>fa,f)()
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(identity)
  }

  def flatMap3[A,B](fa: F[A])(f: A => F[B]): F[B] = {
    join(map(fa)(x=>f(x)))
  }

  def compose2[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => {
    join(map(f(a))(g))
  }
}

object Monad {
  def ParMonad[A]: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]) = Par.flatMap(ma)(f)
  }

  def OptionMonad[A]: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]) = ma match {
      case Some(a) => f(a)
      case _ => None
    }
  }

  def StreamMonad[A]: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) = ma.flatMap(f)
  }

  def ListMonad[A]: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: (A) => List[B]) = ma.flatMap(f)
  }

  def ParserMonad[Parser[+_]](parsers: Parsers[_,Parser]): Monad[Parser] = new Monad[Parser] {
    def unit[A](a: => A) = parsers.zero(a)

    def flatMap[A, B](ma: Parser[A])(f: (A) => Parser[B]) = parsers.flatMap(ma)(f)
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A) = State.unit(a)

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]) = ma.flatMap(f)
  }

  case class Id[A](value: A)

  def idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = Id(a)

    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]) = f(ma.value)
  }

  case class Reader[R,A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

      def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R,B] = {
        Reader(r=>(f(ma.run(r))).run(r))
      }
    }
  }
}