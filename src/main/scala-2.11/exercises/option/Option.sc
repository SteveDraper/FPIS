import exercises.option._

Option(1)
None

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum/xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
  val m = mean(xs)

  m match {
    case None => None
    case Some(mval) => mean(xs.map(x => math.pow(x-mval,2)))
  }
}

def variance2(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else {
    def accumulate(acc: (Double, Double, Int), v: Double) : (Double, Double, Int) = (acc._1+v, acc._2+v*v, acc._3+1)
    val (sum, sumSquares, len) : (Double,Double, Int) = xs.foldLeft((0.0,0.0,0))(accumulate)

    Some(sumSquares/len - sum*sum/(len*len))
  }
}

val nums: Seq[Double] = (1 to 1000).map({x:Int => x.toDouble})

mean(nums)

variance(nums)

variance2(nums)
