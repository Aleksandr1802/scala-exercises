object Section4 {
  // given code
  case class CSome[A](get: A) extends CustomOption[A]

  case object CNone extends CustomOption[Nothing]

  // EXERCISE 4.1: Implement all of the preceding functions on Option
  sealed trait CustomOption[+A] {
    def map[B](f: A => B): CustomOption[B] = this match {
      case CNone        => CNone
      case CSome(value) => CSome(f(value))
    }

    def flatMap[B](f: A => CustomOption[B]): CustomOption[B] = this match {
      case CNone        => CNone
      case CSome(value) => f(value)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case CSome(value) => value
      case CNone        => default
    }

    def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B] = this match {
      case CSome(value) => CSome(value)
      case CNone        => ob
    }

    def filter(f: A => Boolean): CustomOption[A] =
      flatMap((v) => if (f(v)) CNone else CSome(v))
  }

  /*
  EXERCISE 4.2: Implement the variance function in terms of flatMap.
   */

  def std2(s: Seq[Double]): Option[Double] = {
    val mu = s.foldLeft[Double](0.0)((acc, x) => acc + x / s.length)
    val op = s.map(x => Some(math.pow(x - mu, 2)))

    s.length match {
      case x if x < 2 => None
      case k => op.foldLeft[Option[Double]](Some(0.0))((r, x) =>
          for {
            n <- x
            m <- r
          } yield {
            n / (k - 1) + m
          }
        )
  }
  }

  /*
  EXERCISE 4.3
  Write a generic function map2 that combines two Option values using a binary function.
  If either Option value is CNone, then the return value is too.
   */
  def map2Match[A, B, C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] = (a, b) match {
    case (CNone, bv)            => CNone
    case (av, CNone)            => CNone
    case (CSome(av), CSome(bv)) => CSome(f(av, bv))
  }
  def map2[A, B, C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  /*
  EXERCISE 4.4
  Write a function sequence that combines a list of Options into one Option containing
  a list of all the CSome values in the original list. If the original list contains CNone even
  once, the result of the function should be CNone; otherwise the result should be CSome
  with a list of all the values.
   */
  // via Fold
  def sequence[T](a: List[CustomOption[T]]): CustomOption[List[T]] = {
    a.foldLeft[CustomOption[List[T]]](CSome(List[T]()))((lop, op) =>
      (lop, op) match {
        case (CSome(l), CSome(v)) => CSome(v :: l);
        case (CNone, x)           => CNone
        case (x, CNone)           => CNone
      }
    )
  }

  // recursive implementation
  def sequence_rec[T](a: List[CustomOption[T]]): CustomOption[List[T]] = {
    @annotation.tailrec
    def loop(ls: List[CustomOption[T]], lo: List[T]): CustomOption[List[T]] = ls match {
      case CNone :: tail    => CNone
      case Nil              => CSome(lo)
      case CSome(h) :: tail => loop(tail, h :: lo)
    }

    loop(a, List[T]())
  }

  //EXERCISE 4.5: implement  traverse

  def traverse[A, B](a: List[A])(f: A => CustomOption[B]): CustomOption[List[B]] = {
    @annotation.tailrec
    def loop(ls: List[A], lo: List[B]): CustomOption[List[B]] = ls match {
      case Nil => CSome(lo)
      case x :: tail =>
        CSome(x).flatMap(f) match {
          case CNone    => CNone
          case CSome(y) => loop(tail, y :: lo)
        }
    }

    loop(a, List[B]())
  }

  // EXERCISE 4.6:  Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.

  case class CLeft[+E](value: E) extends CEither[E, Nothing]

  case class CRight[+A](value: A) extends CEither[Nothing, A]

  sealed trait CEither[+E, +A] {

    def map[B](f: A => B): CEither[E, B] = this match {
      case CLeft(e)  => CLeft(e)
      case CRight(v) => CRight(f(v))
    }

    def flatMap[EE >: E, B](f: A => CEither[EE, B]): CEither[EE, B] = this match {
      case CLeft(e)  => CLeft(e)
      case CRight(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => CEither[EE, B]): CEither[EE, B] = this match {
      case CLeft(e) => b
      case x        => x
    }

    def map2[EE >: E, B, C](b: CEither[EE, B])(f: (A, B) => C): CEither[EE, C] =
      this.flatMap((x) => b.map(y => f(x, y)))
  }
  /*
  EXERCISE 4.7
  Implement sequence and traverse for Either. These should return the first error
  that’s encountered, if there is one.
   */

  def esequence[E, A](es: List[CEither[E, A]]): CEither[E, List[A]] = {
    es.foldLeft[CEither[E, List[A]]](CRight(List[A]()))((lop, op) =>
      (lop, op) match {
        case (CRight(l), CRight(v)) => CRight(v :: l);
        case (CLeft(e), x)          => CLeft(e)
        case (x, CLeft(e))          => CLeft(e)
      }
    )
  }

  // recursive implementation
  def esequenceRec[E, A](es: List[CEither[E, A]]): CEither[E, List[A]] = {
    @annotation.tailrec
    def loop(ls: List[CEither[E, A]], lo: List[A]): CEither[E, List[A]] = ls match {
      case CLeft(e) :: tail  => CLeft(e)
      case Nil               => CRight(lo)
      case CRight(h) :: tail => loop(tail, h :: lo)
    }

    loop(es, List[A]())
  }

  def etraverse[E, A, B](as: List[A])(f: A => CEither[E, B]): CEither[E, List[B]] = {
    @annotation.tailrec
    def loop(ls: List[A], lo: List[B]): CEither[E, List[B]] = ls match {
      case Nil => CRight(lo)
      case x :: tail =>
        CRight(x).flatMap(f) match {
          case CLeft(e)  => CLeft(e)
          case CRight(r) => loop(tail, r :: lo)
        }
    }

    loop(as, List[B]())
  }
  //  EXERCISE 4.8 it is possible to use List[T] as E, so errors could be stackable now

}
