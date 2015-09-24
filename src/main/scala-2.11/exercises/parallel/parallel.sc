import exercises.parallel.Par._
import java.util.concurrent.ForkJoinPool

def lists(n: Int): List[List[Int]] = {
  (for(i <- 1 to n) yield (1 to i).toList) toList
}

val test = lists(20)

val ex = new ForkJoinPool

def fib(n: Int): Int = {
  if (n==0) 0
  else if (n==1) 1
  else fib(n-1) + fib(n-2)
}

def mapFn(l: List[Int]): Int = { println(ex.getActiveThreadCount); l.sum }

//val m1 = test map (mapFn)
//val m2 = parMap(test)(mapFn)(ex).get
//val m3 = parFilter(test)(l=>{val f = fib(l.length+30); println("%d: %d".format(f,ex.getActiveThreadCount)); f>10})(ex).get()
//m3

parSum((1 to 100).toList,0)(_+_)(ex).get

parRed((1 to 100).toList,0)(_+_)(ex).get


def wordCount(ss: List[String]): Par[Int] = parAgg(ss,0)((s:String) => s.split("\\s+").length)(_+_)

//val wordsList = lists(30) map (_.toString)

//wordCount(wordsList)(ex).get

val fibsCalcList = (1 to 10) map (x => unit(fib(x)))

choiceN(unit(9))(fibsCalcList.toList)(ex).get

choice(unit(false))(unit(fib(5)),unit(fib(0)))(ex).get

chooser(unit(10))(x => unit(fib(x)))(ex).get

flatMap(unit(10))(x => unit(fib(x)))(ex).get

flatMap2(unit(10))(x => unit(fib(x)))(ex).get
