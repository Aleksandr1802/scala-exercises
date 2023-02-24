import Section5.CStream.ccons
import Section5.unfold
object Section5 {

  // Given code ---
  case object Empty extends CStream[Nothing]

  case class Ccons[+A](h: () => A, t: () => CStream[A]) extends CStream[A]

  object CStream {
    def ccons[A](hd: => A, tl: => CStream[A]): CStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Ccons(() => head, () => tail)
    }

    def empty[A]: CStream[A] = Empty

    def apply[A](as: A*): CStream[A] =
      if (as.isEmpty) empty else ccons(as.head, apply(as.tail: _*))
  }

  sealed trait CStream[+A] {
    // generalized functions for iterating by stream, using f(curr_value, acc, tail) and stop(curr_value,acc) as stop criteria
    @annotation.tailrec
    private def iterateStream[B](x: B)(f: (A, => B, => CStream[A]) => B)(stop: (=> A, => B) => Boolean): B = this match {
      case Ccons(h, tl) if stop(h(), x) => tl().iterateStream(f(h(), x, tl()))(f)(stop)
      case _ => x
    }

    def foldLeft[B](x: B)(f: (A, => B) => B): B =
      iterateStream(x)((a, b, _) => f(a, b))((_, _) => true)

    def foldRight[B](x: => B)(f: (A, => B) => B): B = this match {
      case Ccons(h, tl) => f(h(), tl().foldRight(x)(f))
      case _ => x
    }

    // EXERCISE 5.1:     CStream to list
    def toList: List[A] =
      foldLeft(List[A]())((v, acc) => acc ::: List(v))

    // EXERCISE 5.2:     take(n), drop(n) - take first elems, skip/pass first n elems, take should return a stream?
    def take(n: Int): CStream[A] = this match {
      case Ccons(h, tail) if n > 1 => ccons(h(), tail().take(n - 1))
      case Ccons(h, tail) if (n == 1) => ccons(h(), Empty)
      case _ => Empty
    }
    def takeList(n: Int): List[A] =
      iterateStream(List[A]())((v, acc, _) => acc ::: List(v))((_, b) => b.length < n)
    def drop(n: Int): CStream[A] =
      iterateStream(List[CStream[A]](this))((_, acc, tail) => tail :: acc)((_, b) => b.length <= n) match {
        case h :: _ => h
        case Nil => Empty
      }

   //EXERCISE 5.3:   takeWhile(p) - take first ... elems, while p is satisfied
   def takeWhile(p: A => Boolean): CStream[A] = this match {
      case Ccons(h, tail) if p(h()) => ccons(h(), tail().takeWhile(p))
      case _ => Empty
    }

   // EXERCISE 5.4-5.5:   forAll(p) : Bool
    def forAll(p: A => Boolean): Boolean =
      iterateStream(true)((v, acc, s) => p(v) && acc)((_, x) => x)

    def forAllByFold(p: A => Boolean): Boolean =
      foldRight(true)((v, acc) => p(v) && acc)

     // EXERCISE 5.6:   Hard: Implement headOption using foldRight
    def headOption: Option[A] =
      foldRight[Option[A]](None)((v, acc) => Some(v))

    // EXERCISE 5.7:  map, filter, append, and flatMap using foldRight
    def map[B](f: (=> A) => B): CStream[B] =
      foldRight[CStream[B]](Empty)((v, acc) => ccons(f(v), acc))

    def filter(p: (=> A) => Boolean): CStream[A] =
      foldRight[CStream[A]](Empty)((v, acc) => if (p(v)) acc else ccons(v, acc))

    def append[B >: A](s: => CStream[B]): CStream[B] =
      foldRight[CStream[B]](s)((v, acc) => ccons(v, acc))

    def flatMap[B](f: (=> A) => CStream[B]): CStream[B] =
      foldRight[CStream[B]](Empty)((v, acc) => f(v).append(acc))


    //EXERCISE 5.13 Use unfold to implement map, take, takeWhile
    def Umap[B](f: (=> A) => Option[B]): CStream[B] =
      unfold(this)(s => s match {
        case Ccons(h, t) => f(h()).flatMap(x => Some(x, t()));
        case _ => None
      })

    def Utake(n: Int): CStream[A] =
      unfold((this, n))(tup => tup match {
        case (Ccons(h, t), k) => Some((h(), (t(), k - 1))).filter(_ => k > 0);
        case _ => None
      })


    def UtakeWhile(p:A => Boolean): CStream[A] =
      unfold((this, true))(tup => tup match {
        case (Ccons(h, t), true) if p(h())=> Some((h(), (t(), p(h()))))
        case _ => None
      })

    //EXERCISE 5.14 Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
    def startsWith[A](s: CStream[A]): Boolean =
      zipWidth(this, s)((v1, v2) => v1==v2).forAll(v => v==true)


    //EXERCISE 5.15 tails returns the Stream of suffixes of the input sequence, starting with the original Stream. (use unfold!)
    def tails: CStream[CStream[A]] =
      ccons(this, unfold[CStream[A], CStream[A]](this)(s => s match {
        case Ccons(h1, t1) => Some((t1(), t1()))
        case _ => None
      }))


  //EXERCISE 5.16 Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results.
    /*
     PS: I have no idea how to apply RightFold there since it doesn't use a tail recursion ( 'f' called after all stream are processed),
     while FoldLeft calls f every iteration

    */
    def scanLeft[B](x: B)(f: (A, => B) => B): List[B] =
      foldLeft[List[B]](List[B]())((v, acc) => acc ::: List(acc match { case h::t => f(v, h); case Nil => f(v, x)}))



  }

  // EXERCISE 5.8 : infinite stream of cconstants
  def GenStreamConsts[T](c: T): CStream[T] = {
    lazy val s: CStream[T] = ccons(c, s)
    s
  }

  //EXERCISE 5.9 : infinite stream of natural series
  def GenStreamNaturalSeries(n: Int): CStream[Int] = {
    lazy val s: CStream[Int] = ccons(n, s.map(_ + 1))
    s
  }

  //EXERCISE 5.10 :  fibs that generates the infinite stream of Fibonacci numbers: X_n = X_n-1 + X_n-2
  def GenStreamFibonacci(initTuple: (Int, Int) = (0, 1)) : CStream[Int] =
    ccons(initTuple._1, GenStreamFibonacci((initTuple._2, initTuple._1 + initTuple._2)))

  // EXERCISE 5.11 : general correcursive function
  def unfold[A, S](x: S)(f: S => Option[(A, S)]): CStream[A] = f(x) match {
      case Some((v, ac)) =>  ccons(v, unfold(ac)(f))
      case _ => Empty
    }

  /* EXERCISE 5.12 : const, natural, ones, fid in terms of unfold

  unfold(0)(x => Some((x, x + 1))) - natural series
  unfold(0)(x => Some((x, x))) - const
  unfold(0)(x => Some((1,1))) - ones
  unfold[Int, List[Int]](List(0, 1))(l => l match {case m::n::Nil => Some(m, List(n, m + n))}) - fibonacci
  */


  //EXERCISE 5.13 Use unfold to implement zipWith, zipAll (given signature: def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])])
  def zipWidth[A, B, C](s1: CStream[A], s2: CStream[B])(g: (A, B) => C): CStream[C] =
    unfold((s1, s2))(tup => tup match {
      case (Ccons(h1, t1), Ccons(h2, t2)) => Some((g(h1(), h2()), (t1(), t2())))
      case _ => None
    })

  def zipAll[A, B](s1: CStream[A], s2: CStream[B]): CStream[(Option[A], Option[B])] =
    unfold((s1, s2))(tup => tup match {
      case (Ccons(h1, t1), Ccons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (_, Ccons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case (Ccons(h1, t1), _) => Some(((Some(h1()), None)), (t1(), Empty))
      case _ => None
    })





}
