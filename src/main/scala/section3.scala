
object Section3 {

  // EXERCISE 3.2: Remove first element of a List
  def removeFirst[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: t => t
  }

  // EXERCISE 3.3: replacing the first element with x
  def setFirst[T](l: List[T], x: T): List[T] = l match {
    case Nil => List(x)
    case h :: t => List(x) ::: t
  }

  // EXERCISE 3.4: f removes the first n elements from a list
  def dropFirstN[T](l: List[T], n: Int): List[T] = {
    @annotation.tailrec
    def loop(ls: List[T], k: Int): List[T] = k match {
      case 0 => ls
      case ki => loop(removeFirst(ls), ki-1)
    }
    loop(l, n)
  }

  // EXERCISE 3.5: drop elem while f is true
  def dropWhile[T](l: List[T], f: T => Boolean): List[T] = {
    @annotation.tailrec
    def loop(ls: List[T]): List[T] = ls match {
      case Nil => ls
      case h :: t if f(h) => loop(t)
    }
    loop(l)
  }

  // EXERCISE 3.6:
  /*
  Some comments:
   "Why can’t this function be implemented in constant time like  tail?"
      - obviously, because there is one-directional linked list is being used.
      for the bidirectional, is is possible to iterate from tail as well.
   */
  def dropLast[T](l: List[T]): List[T] = {
    @annotation.tailrec
    def loop(l1: List[T], l2: List[T]): List[T] = l2 match {
      case Nil => l1
      case h :: t :: Nil => l1 ::: List(h)
      case h :: t => loop(l1 ::: List(h), t)
    }
    loop(List[T](), l)
  }
  /*
   EXERCISE 3.7: "Can product, implemented using foldRight, immediately halt the recursion and
  return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
  might work if you call foldRight with a large list. This is a deeper question that we’ll
  return to in chapter 5."

   No, since foldRight are processing all the nodes of list strictly before calling an aggregate function.
   With this specific implementation foldRight will fall to StackOverflow for long lists
   */

  // EXERCISE 3.8 Compute the length of a list using foldRight

  // given
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  // EXERCISE 3.9: Compute the length of a list using foldRight.
  def lenR[T](l: List[T]): Int = {
    foldRight(l, 0)((_, k) => k + 1)
  }

  // EXERCISE 3.10:  write foldLeft
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], x: B)(f: (A, B) => B): B = l match {
    case Nil => x
    case h :: tail => foldLeft(tail, f(h, x))(f)
  }

  // EXERCISE 3.11: Write sum, product, and a function to compute the length of a list using foldLeft
  val sumup = (l: List[Double]) => foldLeft(l, 0.0)(_ + _): Double
  val prod = (l: List[Double]) => foldLeft(l, 1.0)(_ * _): Double
   def lenL[T](l: List[T]): Int =  foldLeft(l, 0)((_, k) => k + 1)


  // EXERCISE 3.12: without using fold
  def reverseList[T](l: List[T]): List[T] = {
    @annotation.tailrec
    def loop(l1: List[T], l2: List[T]): List[T] = l2 match {
      case Nil => l1
      case h :: t => loop(h :: l1, t)
    }

    loop(List[T](), l)
  }

  // EXERCISE 3.12, by fold
  def reverseListF[T](l: List[T]): List[T] =
    foldLeft(l, List[T]())((x: T, z: List[T]) => List(x) ::: z)


  // EXERCISE 3.13:  Can you write foldLeft in terms of foldRight?
def foldRightByLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  foldLeft(reverseListF(as), z)(f)


  // EXERCISE 3.14 Implement append in terms of either foldLeft or foldRight.
  // ( insert at the top)
  def appendTop[T](l: List[T], el: T): List[T] =
    foldLeft(l, List[T](el))((x: T, z: List[T]) => z ::: List(x))

  // (insert at the bottom)
  def appendBot[T](l: List[T], el: T): List[T] =
    foldRight(l, List[T](el))((x: T, z: List[T]) => List(x) ::: z)

  // (insert LIST at the bottom)
  def appendList[T](l: List[T], ls: List[T]): List[T] =
    foldRight(l, ls)((x: T, z: List[T]) => List(x) ::: z)


  // EXERCISE 3.15; Write a function that concatenates a list of lists into a single list.
  def ravel[T](ll: List[List[T]], out: List[T]): List[T] = ll match {
    case Nil => out
    case x :: Nil => appendList(out, x)
    case x :: tail => ravel(tail, appendList(out, x))
  }

  // EXERCISE 3.16, Write a function that transforms a list of integers by adding 1 to each element.
  // *** more general implementation. To make +1 increment, f should be: (x)=> x + 1
  def applyToEach[T](ls: List[T], f: (T) => T): List[T] =
    foldLeft(ls, List[T]())((x: T, z: List[T]) => z ::: List(f(x)))


  // EXERCISE 3.17: Write a function that turns each value in a List[Double] into a String
  // *** more general solution, there f should be: (x)=> x.toString()
  def listToStrings[T](ls: List[T], f: (T) => String): List[String] =
    foldLeft(ls, List[String]())((x: T, z: List[String]) => z ::: List(f(x)))

// EXERCISE 3.18: map
  def map[A, B](ls: List[A])(f: A => B): List[B] =
    foldLeft(ls, List[B]())((v, acc) => acc ::: List(f(v)))


  // EXERCISE 3.19: filter
  def filter[T](l: List[T])(f: T => Boolean): List[T] =
    foldLeft(l, List[T]())((v, acc) => if (f(v)) acc ::: List(v) else acc)


  // EXERCISE 3.20: flatMap
 def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] =
   foldLeft(ls, List[B]())((v, acc) => acc ::: f(v))


  //  EXERCISE 3.21:  Use flatMap to implement filter
  def filterFM[T] (l: List[T])(f: T => Boolean): List[T] =
    flatMap(l)((x:T) => if (f(x)) List[T]() else List[T](x))

  // EXERCISE 3.22-3.23, zipWith
  def zipWidth[T](l1:List[T], l2:List[T])(f: (T, T) => T): List[T] = {
    @annotation.tailrec
    def go(ls1: List[T], ls2: List[T], out: List[T]): List[T] = (ls1, ls2) match {
      case (Nil, lst) => out
      case (lst, Nil) => out
      case (h :: t, u :: v) => go(t, v, out ::: List[T](f(h, u)))
    }
    go(l1, l2, List[T]())
  }

// EXERCISE 3.24, is there a subsequence?
  def findSubseq(sup: List[Int], sub: List[Int]): Boolean = {
    val comp = (c:Int) => map(sup)((x)=> if (x==c) 1; else 0) // comparing function
    @annotation.tailrec
    def go(mask: List[Int], s: List[Int]): List[Int] = s match {
      case Nil => mask
      case c :: tail => {
        val new_mask = comp(c)   // check current char c along input
        val prod_mask = zipWidth[Int](0::mask,  new_mask)((a,b)=> a * b)  // logical AND with prev mask, shifted before
        go(prod_mask, tail)
      }
    }
   (sub match {
      case c::Nil  => comp(c)
      case c::tail => go(comp(c), tail)
      case _ => List(0)
    }).foldLeft(0)(_+_) > 0
  }


  // Trees: given code
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]



  /// EXERCISE 3.25, count of nodes
  def treeSize[T](t : Tree[T]): Int = {
    def passTree(t: Tree[T], acc: Int): Int = t match {
      case t: Leaf[T] => acc + 1
      case t: Branch[T] => 1 + passTree(t.left, acc) + passTree(t.right, acc)
    }
    passTree(t, 0)
  }


  /// EXERCISE 3.26, min/max value
  def treeMInMaxValue[T](t: Tree[T], thresh: T)(fmax: (T, T) => T): T = {
    def passTree(t: Tree[T], v: T): T = t match {
      case t: Leaf[T] => t.value
      case t: Branch[T] =>  fmax(passTree(t.left, v), passTree(t.right, v))
    }

    passTree(t, thresh)
  }

  /// EXERCISE 3.27, depth
  // recursively pass by nodes and count only branches
  def tree_depth[T](t: Tree[T]): Int = {

    def passTree(t: Tree[T], acc: Int): Int = t match {
      case t: Leaf[T] => acc
      case t: Branch[T] => 1 + passTree(t.left, acc) max passTree(t.right, acc)
    }

    passTree(t, 0)
  }

 // EXERCISE 3.28 map tree

  def map[T, H](t: Tree[T])(f: T => H): Tree[H] = {

    def passTree(t: Tree[T]): Tree[H] = t match {
      case t: Leaf[T] => Leaf[H](f(t.value))
      case t: Branch[T] => Branch[H](passTree(t.left), passTree(t.right))
    }
    passTree(t)
  }


  // EXERCISE 3.29   Generalize size, maximum, depth, and map, writing a new function fold
  def foldTree[T, H](t: Tree[T], v: H)(f: (T, H) => H, g: (H, H)=> H): H = t match {
    case t: Leaf[T] => f(t.value, v)
    case t: Branch[T] => g(foldTree(t.left, v)(f, g), foldTree(t.right, v)(f, g))
  }
  /*
  implementation of functions via fold:

  1. foldTree(tr1, 0)((x, y)=> if (x < y) y; else x, (x, y)=> x max y) - max value
  2. foldTree(tr1, 0)((x, y)=> y + 1, (x, y)=> x + y + 1) - size of tree
  3. foldTree(tr2, 0)((x, y)=> y, (x, y)=> (x max y) + 1) - tree depth
  4. foldTree[A, Section3.Tree[T]](tr2, Section3.Leaf[T](0))((x, y)=> Section3.Leaf(f(x)), (x, y) => Section3.Branch[T](x, y) - map f by over the tree

  */

}

