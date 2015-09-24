package exercises.parallel


import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] = a => {
    lazyUnit(f(a))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit())((a, _) => f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    def foldFn(pa: Par[A], acc: Par[List[A]]): Par[List[A]] = map2(pa, acc)(_ :: _)

    ps.foldRight(unit(Nil: List[A]))(foldFn)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps map (asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    def stripNone(os: List[Option[A]]): List[A] = {
      for (o <- os if o.isDefined) yield o.get
    }

    def filterMapFn(a: A): Option[A] = {
      if (f(a)) Some(a) else None
    }


    val fs: List[Par[Option[A]]] = as map (asyncF(filterMapFn))
    map(sequence(fs))(stripNone)
  }

  def parSum[A](as: List[A], u: A)(f: (A, A) => A): Par[A] = {
    if (as.length <= 1)
      Par.unit(as.headOption getOrElse u)
    else {
      val (l, r) = as splitAt as.length / 2
      map2(fork(parSum(l, u)(f)), fork(parSum(r, u)(f)))(f(_, _))
    }
  }

  def parAgg[A, B](as: List[A], u: B)(f: A => B)(g: (B, B) => B): Par[B] = {
    val len = as.length
    if (len == 0)
      Par.unit(u)
    if (len == 1)
      Par.unit(f(as.head))
    else {
      val (l, r) = as splitAt as.length / 2
      map2(fork(parAgg(l, u)(f)(g)), fork(parAgg(r, u)(f)(g)))(g(_, _))
    }
  }

  def parRed[A](as: List[A], u: A)(f: (A, A) => A): Par[A] = {
    parAgg(as, u)(a => a)(f)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val indexp = map(cond)(c => if (c) 0 else 1)
    choiceN(indexp)(List(t, f))
  }

  def choiceN[A](cond: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(cond)(x => choices(x))

  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      f(a)(es)
    }

  def join[A](pa: Par[Par[A]]): Par[A] =
    es => {
      run(es)(run(es)(pa).get())
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    val mapped = map(pa)(f)
    join(mapped)
  }

  def flatMap2[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join2(map(pa)(f))

  def join2[A](pa: Par[Par[A]]): Par[A] = flatMap(pa)((x:Par[A])=>x)
}