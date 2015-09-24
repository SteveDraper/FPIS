package exercises
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop._
import org.scalacheck.Gen
import exercises.temperature._
import Temperature._

/**
 * Created by steve on 9/17/2015.
 */
class Test extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "Temperature" should "be idempotent in Fahrenheit" in {
    val anIntegerTemp = 100 Celsius

    assert((anIntegerTemp asF) == ((anIntegerTemp asF).Fahrenheit asF))
  }
  it should "be idempotent in Celsius" in {
    val anIntegerTemp = 100 Celsius

    assert((anIntegerTemp asC) == ((anIntegerTemp asC).Celsius asC))
  }
  it should "be idempotent in Kelvin" in {
    val anIntegerTemp = 100 Celsius

    assert((anIntegerTemp asK) == ((anIntegerTemp asK).Kelvin asK))
  }
  it should "back convert" in {
    val startTemp = 100.0 Celsius
    val inF = startTemp asF

    assertResult(100.0) {
      (inF Fahrenheit) asC
    }
  }
  it should "back convert celsius as int" in {
    val startTemp = 100 Celsius
    val inF = startTemp asF

    assertResult(100) {
      (inF Fahrenheit) asC
    }
  }
  it should "back convert celsius as double" in {
    val startTemp = 100.0 Celsius
    val inF = startTemp asF

    assert {
      Math.abs(((inF Fahrenheit) asC) - 100.0) < 0.00001
    }
  }
  it should "back convert fahrenheit as double" in {
    val startTemp = 100.0 Fahrenheit
    val inC = startTemp asC

    assert {
      Math.abs(((inC Celsius) asF) - 100.0) < 0.00001
    }
  }
  it should "preserve int units" in {
    val startTemp = 100 Celsius

    assert(startTemp asF match {
      case x:Int => true
      case _ => false
    })
  }
  it should "preserve long units" in {
    val startTemp = 100L Celsius

    assert(startTemp asF match {
      case x:Long => true
      case _ => false
    })
  }
  it should "preserve double units" in {
    val startTemp = 100.0 Celsius

    assert(startTemp asF match {
      case x:Double => true
      case _ => false
    })
  }

  val smallInts = for (n <- Gen.choose(-1000, 1000)) yield n

  "temps above absolute zero" should "be preserved across unit conversions" in {
    forAll { (t: Int) =>
      whenever(t > 0) {
        val temp = t.toDouble Kelvin

        (((temp asC) Celsius) asK) shouldEqual t.toDouble
      }
    }
  }
  it should "round correctly in int units" in {
    forAll(smallInts) { (t: Int) =>
      whenever(t > 0) {
        val temp = t Fahrenheit

        (((temp asK) Kelvin) asF) shouldEqual t +- 1
      }
    }
  }
}
