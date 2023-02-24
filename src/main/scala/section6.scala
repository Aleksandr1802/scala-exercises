

import scala.util.Random
object Section6 {


  // EXERCISE 6.1
  // Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
  // Solution slightly differ, The rng class was added
  case class NRNG(seed: Long) {
    def getInt: (Int, NRNG) = {
      val defaultRng = new Random(seed=seed)
      val n = defaultRng.nextInt()
      val newRng = new NRNG(defaultRng.nextInt())
      (n, newRng)
    }
    def getDouble: (Double, NRNG) = {
      val defaultRng = new Random(seed = seed)
      val d = defaultRng.nextDouble()
      val newRng = new NRNG(defaultRng.nextInt())
      (d, newRng)
    }
  }
  def nonNegativeInt(rng: NRNG): (Int, NRNG) = {
    val t = rng.getInt
    (t._1.abs % Int.MaxValue, t._2)
  }

  //   EXERCISE 6.2
  // Double between 0 and 1, not including 1
  def randDouble(rng: NRNG): (Double, NRNG) =
    rng.getDouble


  //EXERCISE 6.3:  Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a  (Double, Double, Double)
  def intDouble(rng: NRNG): ((Int,Double), NRNG) = {
    val t1 = rng.getInt
    val t2 = t1._2.getDouble
    ((t1._1, t2._1), t2._2)
  }
  def doubleInt(rng: NRNG): ((Double,Int), NRNG) = {
    val t1 = rng.getInt
    val t2 = t1._2.getDouble
    ((t2._1, t1._1), t2._2)
  }
  def double3(rng: NRNG): ((Double,Double,Double), NRNG) = {
    val t1 = rng.getDouble
    val t2 = t1._2.getDouble
    val t3 = t2._2.getDouble
    ((t1._1, t2._1, t3._1), t3._2)
  }

//  EXERCISE 6.4:  Write a function to generate a list of random integers.
  def ints(count: Int)(rng: NRNG): (List[Int], NRNG) = {
    def go(l: List[Int], n: Int, rngCurr:NRNG): (List[Int], NRNG) = n match {
      case 0 => (l, rngCurr)
      case k => {
        val rn1 = rngCurr.getInt
        go(rn1._1 :: l, k - 1, rn1._2)
      }
    }
    go(List[Int](), count, rng)
  }

  // given::
  type RandGen[+A] = NRNG => (A, NRNG)
  def map[A, B] (r: RandGen[A])(f: A => B): RandGen[B] = rng => {
    val (v, rng2) = r(rng)
    (f(v), rng2)
  }

// EXERCISE 6.5 Use map to reimplement double in a more elegant way.
  def randDouble2: RandGen[Double] =
    map(nonNegativeInt)(v => v.toDouble / Int.MaxValue)

  // EXERCISE 6.6: Write the implementation of map2 based on the following signature.
  def map2[A,B,C](ra: RandGen[A], rb: RandGen[B])(f: (A, B) => C): RandGen[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a,b), rngB)
  }


  // EXERCISE 6.7 - Hard: Implement sequence for combining a List of transitions into a single transition. Use it to reimplement the ints function you wrote before.
  def sequence[A](fs: List[RandGen[A]]): RandGen[List[A]] = {

    def go(tail: List[RandGen[A]], acc: List[A], lastRng:NRNG): (List[A],NRNG)  = tail match {
      case h::t => {
        val (v, rngV) = h(lastRng)
        go(t, v :: acc, rngV)
      }
      case Nil => (acc, lastRng)
    }
    rng => go(fs, List[A](), rng)
  }


  // EXERCISE 6.8:  implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: RandGen[A])(g: A => RandGen[B]): RandGen[B] = rng => {
    val (v, rng2) = f(rng)
    g(v)(rng2)
  }

  // EXERCISE 6.9 - 6.10
  //Reimplement map and map2 in terms of flatMap.
  // Generalize the functions unit, map, map2, flatMap, and sequence.
  // Add them as methods on the State case class where possible. Otherwise you should put them in a State companion object

  case class State[S,+T](run: S => (T,S)){
  def unit[B](v: B): State[S, B] = State((is:S) => (v, is))

  def flatMap[B](f: T => State[S, B]): State[S, B] = State(s => {
      val (v, s1) = this.run(s)
      f(v).run(s1)
    })

    // in terms of flatmap*
  def map[B](f: T => B): State[S, B] = flatMap((v:T) => State((s:S) => (f(v), s)))

  def map2[A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C] ((s:S) => {
    val (a, s1) = flatMap(v => sa).run(s)
    val (b, s2) = flatMap(v => sb).run(s1)
    (f(a, b), s2)
  })

  def sequence[B >: T](fs: List[State[S, B]]): State[S,List[B]] = State((s:S) => {
    def go(tail: List[State[S, B]], acc: List[B], ls: S): (List[B], S) = tail match {
      case h :: t => {
        val (v, s1) = h.run(ls)
        go(t, v :: acc, s1)
      }
      case Nil => (acc, ls)
    }
    go(fs, List[B](), s)
  })
  }



/*



   //EXERCISE 6.11
   Hard: To gain experience with the use of State, implement a finite state automaton
   that models a simple candy dispenser. The machine has two types of input: you can
   insert a coin, or you can turn the knob to dispense candy. It can be in one of two
   states: locked or unlocked. It also tracks how many candies are left and how many
   coins it contains.

   sealed trait Input

   case object Coin extends Input
   case object Turn extends Input
   case class Machine(locked: Boolean, candies: Int, coins: Int)

   The rules of the machine are as follows:
    Inserting a coin into a locked machine will cause it to unlock if there’s any
   candy left.
    Turning the knob on an unlocked machine will cause it to dispense candy and
   become locked.
    Turning the knob on a locked machine or inserting a coin into an unlocked
   machine does nothing.
    A machine that’s out of candy ignores all inputs.


   The method simulateMachine should operate the machine based on the list of inputs
   and return the number of coins and candies left in the machine at the end. For example,
   if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully
    bought, the output should be (14, 1).

   def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]

    */

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)
  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]

}