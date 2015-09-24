package exercises

import exercises.MonoidInstancesTest._
import exercises.monoid.{Part, Stub, Monoid}
import exercises.monoid.Monoid._
import monoid.WC.wcMonoid
import org.scalacheck.Gen.Parameters
import org.scalacheck.{Prop, Arbitrary, Gen}
import org.scalactic.Equality
import org.scalatest.{Suites, Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Arbitrary._

import scala.util.Random

/**
 * Created by steve on 9/19/2015.
 */

class MonoidInstancesTest extends Suites(
  new IntAddition,
  new IntMultiplication,
  new StringConcatenation,
  new BooleanOr,
  new BooleanAnd,
  new OptionFirst,
  new FnComp,
  new WCCat
)

object MonoidInstancesTest {
  private class MonoidLaws extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

    def MonoidTester[A](m: Monoid[A])(gen: Gen[A])(implicit e: Equality[A]) = {
      //implicit val myEquality = e

      "monoid op" should "be associative" in {
        forAll(gen, gen, gen) { (a1: A, a2: A, a3: A) =>
          m.op(m.op(a1, a2), a3) shouldEqual m.op(a1, m.op(a2, a3))
        }
      }
      "zero" should "close to left identity" in {
        forAll(gen) { (a: A) =>
          m.op(m.zero, a) shouldEqual a
        }
      }
      it should "close to right identity" in {
        forAll(gen) { (a: A) =>
          m.op(a, m.zero) shouldEqual a
        }
      }
    }

    def GenericMonoidLaws[A](m: Monoid[A])(gen: Gen[A]) {
      MonoidTester(m)(gen)
    }

    def GenericMonoidLaws[A](m: Monoid[(A=>A)])(gen: Gen[(A=>A)],genP: Gen[A]) {
      def isEqual(f1: A=>A, f2: A=>A): Boolean = {
        Prop.forAll(genP)(a => f1(a) == f2(a)).apply(new Parameters {val size: Int = 10
          val rng: Random = new Random()
        }).success
      }
      implicit val Fn1Equality = new Equality[(A=>A)] {
        def areEqual(fa1: (A)=>A, a2: Any): Boolean = a2 match {
          case fa2:(A=>A) => isEqual(fa1,fa2)
          case _ => false
        }
      }
      MonoidTester(m)(gen)(Fn1Equality)
    }
  }

  private class IntAddition extends MonoidLaws {
    GenericMonoidLaws(intAddition)(Gen.choose(Int.MinValue, Int.MaxValue))
  }
  private class IntMultiplication extends MonoidLaws {
    GenericMonoidLaws(intMultiplication)(Gen.choose(Int.MinValue, Int.MaxValue))
  }
  private class StringConcatenation extends MonoidLaws {
    GenericMonoidLaws(stringMonoid)(arbitrary[String])
  }
  private class BooleanOr extends MonoidLaws {
    GenericMonoidLaws(booleanOr)(arbitrary[Boolean])
  }
  private class BooleanAnd extends MonoidLaws {
    GenericMonoidLaws(booleanAnd)(arbitrary[Boolean])
  }
  private class OptionFirst extends MonoidLaws {
    GenericMonoidLaws(optionMonoid:Monoid[Option[Int]])(arbitrary[Option[Int]])
  }
  private class FnComp extends MonoidLaws {
    GenericMonoidLaws(endoMonoid[Int])(arbitrary[Int=>Int],arbitrary[Int])
  }
  private class WCCat extends MonoidLaws {
    GenericMonoidLaws(wcMonoid) {
      for {
        genPart <- arbitrary[Boolean]
        lstub <- Gen.identifier
        n <- arbitrary[Int]
        rstub <- Gen.identifier
      } yield if (genPart) Part(lstub,n,rstub) else  Stub(lstub)
    }
  }
}