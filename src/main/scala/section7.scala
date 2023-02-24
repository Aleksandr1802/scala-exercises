object Section7 {

  // EXERCISE 7.2: try to come up with representations for Par that make it possible to implement the functions of our API .
  case class Parallel[T](value: T) {
    def run: T = value
  }

  object Tools {
    def unit[T](v: T): Parallel[T] = Parallel(v)
    def lazyUnit[T](v: => T): Parallel[T] = fork(unit(v))

    def run[T](p: Parallel[T]): T = p.run
    def fork[T](v: => Parallel[T]): Parallel[T] = v

    // EXERCISE 7.1:  map2 is a new higher-order function for combining the result of two parallel computations.
    def map2[A, B, Y](p1: Parallel[A], p2: Parallel[B])(f: (A, B) => Y): Y =
      f(run(p1), run(p2))








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

}