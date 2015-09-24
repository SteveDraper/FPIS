def genRecur[A](a: A)(before: A => Either[A,A], after: (A,A) => A): A = {
  def manStack(a: A, stack: List[A=>A]): (A,List[A=>A]) = {
    def afterTransform(inter_a: A): A = after(a, inter_a)
    val inter = before(a)
    if ( inter.isLeft ) (inter.left.get,stack)
    else manStack(inter.right.get, afterTransform _ :: stack)
  }
  val (value, transforms) = manStack(a, Nil)
  transforms.foldLeft(value){(v: A,t: A=>A)=>t(v)}
}
def factorial(n: BigInt): BigInt = genRecur(n)(x=>if(x==1) Left(BigInt(1)) else Right(x-1),_*_)
factorial(100000)