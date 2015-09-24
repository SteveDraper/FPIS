import exercises.temperature._
import Temperature._

val anIntegerTemp = 5 Celsius
val aDoubleTemp = 451.0 Fahrenheit

anIntegerTemp asF

anIntegerTemp asK

aDoubleTemp asC

def backConvCviaF(tC: Int): Int = (((tC Celsius) asF) Fahrenheit) asC
def backConvFviaC(tC: Int): Int = (((tC Fahrenheit) asC) Celsius) asF

backConvCviaF(1101)
backConvFviaC(110)

(0 to 1000) filter(x => (x != backConvFviaC(x)))