import exercises.monad.Monad._

val lm1 = ListMonad.unit(1)

val ll = List(List(1),List(2),List(3))

ListMonad.sequence(ll)

val lo = List(Some(1),Some(2),Some(3),Some(4))

OptionMonad.sequence((lo))

OptionMonad.traverse(List(1,2,3))(Some(_))

OptionMonad.replicateN(5, Some(1))

ListMonad.replicateN(5, List(1,2,3))

ListMonad.map2(List(1,2,3),List(4,5,6))(_.toString+_.toString)

OptionMonad.filterM2((1 to 9).toList)(x=>(if(x<10) Some(x%2==0) else None))

def funnyList(b: Boolean) = ListMonad.filterM((1 to 4).toList)(x=>List(b,b, true))

funnyList(true).toSet
(funnyList(false).toSet)