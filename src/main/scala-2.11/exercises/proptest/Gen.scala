package exercises.proptest

import exercises.stateful.{State,RNG}




/**
 * Created by steve on 9/13/2015.
 */
case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen.map(this)(f)
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap { n => Gen.listOfN(n, this)}
}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: Int) extends Result {
  def isFalsified = true
}

case class Prop(name: String, run: (TestCases,RNG) => Result) {
  def &&(p: Prop) =
    Prop("Combo", {
      (n, rng) => {
        val result = run(n, rng)
        result match {
          case Passed => p.run(n, rng)
          case Falsified(f,n) => result
        }
      }
    })
  def ||(p: Prop) =
    Prop("Combo", {
      (n, rng) => {
        val result = run(n, rng)
        result match {
          case Passed => result
          case Falsified(f,n) => p.run(n, rng)
        }
      }
    })
}

object Gen {
  import RNG._
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(nonNegativeLessThan(stopExclusive-start).map(x => x+start))
  }

  def double(maxVal: Double): Gen[Double] = Gen(RNG.double) map { x => x*maxVal }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(nonNegativeLessThan(2).map(x=>x%2==0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val genList = List.fill(n)(g.sample)

    Gen(State.sequence(genList))
  }

  def map[A,B](g: Gen[A])(f: A => B): Gen[B] = {
    Gen(g.sample.map(f))
  }

  def flatMap[A,B](g: Gen[A])(f: A => Gen[B]): Gen[B] = {
    Gen(g.sample.flatMap(a => f(a).sample))
  }

  def map2[A,B,C](g: Gen[A], g2: Gen[B])(f: (A,B) => C): Gen[C] = {
    flatMap(g)(a => map(g2)(b => f(a,b)))
  }

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int,Int)] = map2(choose(start,stopExclusive), choose(start,stopExclusive))((_,_))

  def char: Gen[Char] = map(choose(32,128))(c=>c.toChar)

  def string(len: Int): Gen[String] = {
    val charListGen = List.fill(len)(char.sample)

    map(Gen(State.sequence(charListGen)))(_.mkString)
  }

  def union[A](g: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap { b => if (b) g else g2 }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen.double(g1._2+g2._2) flatMap { x => if (x > g1._2) g2._1 else g1._1 }
}