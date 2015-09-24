package exercises


import java.util.concurrent.ForkJoinPool

import exercises.monoid.Monoid._
import exercises.parsers._
import MyParsers._

import lazyness.{MyStream}

/**
 * Created by steve on 9/6/2015.
 */
object TestApp extends App {
  val ls = (1 to 10000000)

  ls.sum
  /*foldMap(ls, stringMonoid)(_.toUpperCase())
  ls.foldLeft("")((acc,s) => acc + s.toUpperCase())
  myFoldRight(ls)("")((s,acc) => s.toUpperCase()+acc)
  myFoldLeft(ls)("")((acc,s) => acc + s.toUpperCase())
  foldMap(ls.toIndexedSeq, stringMonoid)(_.toUpperCase())*/

  val parFoldedList = parFoldMap(ls, intAddition)((x:Int)=>x)
  val ex = new ForkJoinPool
  println(parFoldedList(ex).get)

}
