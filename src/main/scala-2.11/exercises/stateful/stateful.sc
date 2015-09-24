import exercises.stateful.SimpleRNG
import exercises.stateful.RNG._
import exercises.monad.Monad._

val startRng = new SimpleRNG(1)

double3.run(startRng)
ints(10)(startRng)
ints2(10).run(startRng)

intDouble.run(startRng)
doubleInt.run(startRng)

nonNegativeInt.run(startRng)
nonNegativeLessThan(1000).run(startRng)

import exercises.stateful.Machine
import exercises.stateful.Coin
import exercises.stateful.Turn
import exercises.stateful.Machine._

val startState = Machine(true,2,0)

val ops = List(Coin,Turn)

val after = simulateMachine(ops).run(startState)

val msMonad = stateMonad[Machine]

val coinTransition = simulateMachine(List(Coin))
val turnTransition = simulateMachine(List(Turn))

val coinAndTurn = msMonad.map2(coinTransition,turnTransition)(List(_,_))

coinAndTurn.run(startState)
msMonad.sequence(List(coinTransition, turnTransition)).run(startState)