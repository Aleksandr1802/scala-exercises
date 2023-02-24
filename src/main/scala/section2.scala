
object Section2 {
  // Exercise 2.1
  // returns n-th fibonacci number
  // just to remind: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233,
  def fibNth(n: Int): Int = {
    @annotation.tailrec
    def loop(n0: Int, n1: Int, k: Int): Int = {
      if (k == 0) n0
      else if (k == 1) n1
      else loop(n1, n0 + n1, k - 1)
    }

    loop(0, 1, n)
  }

  def findElement[T](arr: List[T], el: T): Int = {
    @annotation.tailrec
    def loop(idx: Int): Int = {
      if (idx >= arr.length) -1
      else if (arr(idx) == el) idx
      else loop(idx + 1)
    }
    loop(0)
  }

  // Exercise 2.2
  def isSorted[T](a: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(idx: Int): Boolean = {
      if (idx >= a.length - 1) true
      else if (!ordered(a(idx), a(idx + 1))) false
      else loop(idx + 1)
    }
    loop(0)
  }

  // Exercise 2.3, currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Exercise 2.4, un-currying
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5, composing
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}

