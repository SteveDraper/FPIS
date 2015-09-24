package exercises.stateful

/**
 * Created by steve on 9/9/2015.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S,+A](run: S => (A,S))
{
  import State._

  def flatMap[B](g: A => State[S,B]): State[S,B] = new State(s => {
    val (a, s2) = run(s)
    g(a).run(s2)
  })

  def map[B](f: A => B): State[S,B] = {
    flatMap { a => unit(f(a)) }
  }

}

object State {
  def unit[A,S](a: A): State[S,A] = State(s => (a, s))

  def map2[A,B,C,S](ra: State[S,A], rb: State[S,B])(f: (A,B) => C): State[S,C] = {
    rb.flatMap{ b => ra map { a => f(a,b)}}
  }

  def sequence[A,S](fs: List[State[S,A]]): State[S,List[A]] = {
    def foldFn(acc: State[S,List[A]], s: State[S,A]): State[S,List[A]] = {
      map2(s,acc){_::_}
    }

    fs.foldLeft(unit(Nil):State[S,List[A]])(foldFn)
  }
}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  import State._

  type Rand[+A] = State[RNG, A]

  def nonNegativeInt: Rand[Int] = {
    def nonNegativeIntGen(rng: RNG): (Int, RNG) = {
      val (candidate, newRng) = rng.nextInt
      if (candidate == Int.MinValue) nonNegativeIntGen(newRng) else (candidate.abs, newRng)
    }
    State(nonNegativeIntGen)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    def acceptanceMapping(x: Int) : Rand[Int] = {
      if (x < Int.MaxValue - Int.MaxValue%n) unit(x%n)
      else nonNegativeLessThan(n)
    }
    nonNegativeInt flatMap acceptanceMapping
  }


  def int: Rand[Int] = State(rng => rng.nextInt)

  def ints2(count: Int): Rand[List[Int]] = {
    val genList: List[Rand[Int]] = List.fill(count)(int)

    sequence(genList)
  }


  def double: Rand[Double] = {
    nonNegativeInt map (x => x.toDouble/(Int.MaxValue.toDouble+1))
  }

  def intDouble: Rand[(Int,Double)] = {
    map2(nonNegativeInt, double)((_,_))
  }

  def doubleInt: Rand[(Double, Int)] = {
    map2(double, nonNegativeInt)((_,_))
  }

  def double3: Rand[(Double,Double,Double)] = {
    val two: Rand[(Double,Double)] = map2(double,double) { (_,_) }

    map2(double, two) { (x,y) => (x,y._1,y._2) }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def generate(soFar: (List[Int], RNG), n: Int): (List[Int], RNG) = {
      if (n==0) soFar
      else {
        val next = soFar._2.nextInt
        generate((next._1 :: soFar._1, next._2),n-1)
      }
    }
    generate((Nil,rng), count)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._

  type MachineState = State[Machine, (Int,Int)]

  def simulateMachine(inputs: List[Input]): MachineState = {
    def get(s: Machine): (Int, Int) = (s.coins, s.candies)

    def applyInput(input: Input): MachineState = {
      def applyCoin: MachineState = {
        def transition(s: Machine): ((Int, Int), Machine) = {
          val newState = s match {
            case Machine(true,n,_) if (n>0) => Machine(false, s.candies, s.coins+1)
            case _ => s
          }
          (get(newState), newState)
        }
        State(transition)
      }

      def applyTurn: MachineState = {
        def transition(s: Machine): ((Int, Int), Machine) = {
          val newState = s match {
            case Machine(false,n,_) if (n>0) => Machine(true, s.candies-1, s.coins)
            case _ => s
          }
          (get(newState), newState)
        }
        State(transition)
      }

      input match {
        case Coin => applyCoin
        case Turn => applyTurn
      }
    }

    def compose(input: Input, acc: MachineState): MachineState = {
      map2(acc, applyInput(input))((g,_)=>g)
    }

    //val noChange: MachineState = State(s => (get(s),s))

    //inputs.foldRight(noChange)(compose)
    def convert(s: (List[(Int,Int)],Machine)): Machine = s._2
    val composed: Machine => (List[(Int,Int)],Machine) = sequence(inputs map (x => applyInput(x))).run
    //val state2Machine = composed compose convert

    def toMachineState(s: Machine) = {
      val finalState = convert(composed(s))
      (get(finalState),finalState)
    }
    State(toMachineState)
  }
}