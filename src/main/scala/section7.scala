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


}