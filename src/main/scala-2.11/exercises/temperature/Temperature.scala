package exercises.temperature

/**
 * Created by steve on 9/16/2015.
 */
trait ConvertTemp[T] extends (Double => T)
object ConvertTemp {
  def apply[T](f: Double => T) = new ConvertTemp[T] { override def apply(v: Double) = f(v) }
  implicit val convertToInt = apply(x=>Math.round(x).toInt)
  implicit val convertToLong = apply(Math.round _)
  implicit val convertToDouble = apply(identity)
}

sealed case class Temperature[N](private val kelvin: Double)(convert: ConvertTemp[N]) {
  import Temperature._

  override def toString():String = s"$kelvin Kelvin"

  def asKelvin = convert(kelvin)
  def asFahrenheight = convert(ctof(ktoc(kelvin)))
  def asCelsius = convert(ktoc(kelvin))

  def asK = asKelvin
  def asF = asFahrenheight
  def asC = asCelsius
}

object Temperature {

  implicit class TempOps[N](t: N)(implicit n: Numeric[N], convert: ConvertTemp[N]) {
    implicit val Fahrenheit = Temperature.Fahrenheit(t)(n,convert)
    implicit val Celsius = Temperature.Celsius(t)(n,convert)
    implicit val Kelvin = Temperature.Kelvin(t)(n,convert)
  }

  object TempOps {
    //implicit def toTempOps(t: Int): TempOps[Int] = TempOps(t)
    //implicit def toTempOps(t: Long): TempOps[Long] = TempOps(t)
    //implicit def toTempOps(t: Double): TempOps[Double] = TempOps(t)
  }

  private val absoluteZeroC = -273.15
  private def ftoc(f: Double) = (f-32)*5/9
  private def ctok(c: Double) = c - absoluteZeroC
  private[temperature] def ktoc(k: Double) = k + absoluteZeroC
  private[temperature] def ctof(c: Double) = c*9/5 + 32

  private[temperature] def Fahrenheit[N](f: N)(n: Numeric[N], convert: ConvertTemp[N]) = Temperature(ctok(ftoc(n.toDouble(f))))(convert)
  private[temperature] def Celsius[N](c: N)(n: Numeric[N], convert: ConvertTemp[N]) = Temperature(ctok(n.toDouble(c)))(convert)
  private[temperature] def Kelvin[N](c: N)(n: Numeric[N], convert: ConvertTemp[N]) = Temperature(n.toDouble(c))(convert)

  //implicit def toTempOps(t: Int) = TempOps(t)
  //implicit def toTempOps(t: Long) = TempOps(t)
  //implicit def toTempOps(t: Double) = TempOps(t)
}
