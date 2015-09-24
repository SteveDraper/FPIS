package exercises

import exercises.stateful.RNG

/**
 * Created by steve on 9/13/2015.
 */
package object proptest {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(name: String, f: A => Boolean): Prop =
    Prop(name, {
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a,i) => try {
          if (f(a)) Passed else Falsified(s"property $name failed for value ${a.toString}", i)
        } catch { case e: Exception => Falsified(buildMsg(a,name,e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    })

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    def rngStream(r: RNG) : Stream[A] = {
      val (a, newR) = g.sample.run(r)
      a #:: rngStream(newR)
    }
    rngStream(rng)
  }

  def buildMsg[A](s: A, name: String, e: Exception): String =
    s"test case: $s, property $name\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
