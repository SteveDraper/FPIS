import exercises.lazyness._

val stream = MyStream({println("Eval 1");1},{println("Eval 2");2},{println("Eval 3");3})
val realStream = Stream({println("Eval 1");1},{println("Eval 2");2},{println("Eval 3");3})
stream.toList
stream.take(2).toList
def natStream(n: Int): MyStream[Int] = { MyStream.cons(n, natStream(n+1)) }
val naturals = natStream(1)
naturals.take2(5).toList
naturals.drop(5).take(5).toList
def is20(n: Int): Boolean = (n==20)
naturals.exists(is20)
naturals.forAll(x => x<30)
naturals.takeWhile2(x => x<10) toList;
naturals.headOption
naturals.map(x => x*2).take(5).toList
naturals.map2(x => x*3).take(5).toList
naturals.filter(x => (x%2==1)).take(5).toList
naturals.take(2).append(naturals).take(5).toList
naturals.flatMap((x) => MyStream(x, x+1)).take(5).toList
MyStream.constant('a').take(5).toList
MyStream.from(10).take(5).toList
def fibs = MyStream.unfold((0,1))(l => Some((l._1,(l._2, l._1+l._2))))
fibs.take(8).toList
naturals.zipWith(naturals.drop(2))((x,y)=> x+y).take(6).toList
naturals.take(1).zipAll(naturals.take(3)).toList
naturals.startsWith(MyStream(1))
//naturals.take2(30000).hasSubSequence(MyStream(5000,1001))
naturals.take2(10000).filter(x=>(x%2==0)).toList2
naturals.take2(10000).tails.toList2
def natStream2(n: Int): Stream[Int] = n #:: natStream2(n+1)
val naturals2 = natStream2(1)
naturals2.take(10000).toList
naturals2.take(10000)
MyStream.from(1).take2(10000).toList2
def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
  f(z) match {
    case None => Stream.Empty
    case Some((a, s2)) => a #:: unfold(s2)(f)
  }
}
def zipAll[A,B](s: Stream[A], other: Stream[B]): Stream[(Option[A],Option[B])] = {
  def generate(s: (Stream[A],Stream[B])): Option[((Option[A],Option[B]),(Stream[A],Stream[B]))] = s match {
    case (Stream.Empty,Stream.Empty) => None
    case (ah #:: at,bh #:: bt) => Some(((Some(ah),Some(bh)),(at,bt)))
    case (ah #:: at,Stream.Empty) => Some(((Some(ah),None),(at,Stream.Empty)))
    case (Stream.Empty,bh #:: bt) => Some(((None,Some(bh)),(Stream.Empty,bt)))
  }
  unfold((s, other))(generate)
}

def startsWith[A,B >: A](s: Stream[A], ss: Stream[B]): Boolean = {
  zipAll(s,ss).takeWhile(_._2 != None).forall(x => x._1 == x._2)
}

def tails[A](s: Stream[A]): Stream[Stream[A]] = {
  def generator(s: Stream[A]): Option[(Stream[A],Stream[A])] = s match {
    case Stream.Empty => None
    case _ #:: t => Some((s,t))
  }
  unfold(s)(generator)
}

def hasSubSequence[A,B >: A](s: Stream[A], ss: Stream[B]): Boolean = tails(s) exists (startsWith(_, ss))

//hasSubSequence(naturals2.take(10000),Stream(9000,9001))

def longNaturals(n: Long): Stream[Long] = 1L #:: longNaturals(n+1)

longNaturals(1L).exists(x=>(x==10L))