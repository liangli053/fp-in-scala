package fpinscala.datastructures


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/**
  * Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  * which may be `Nil` or another `Cons`.
  */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  /**
    * Intuitively, why is foldLeft tail-recursive and foldRight not?
    * Because the list is essentially singly linked (h :: t) and foldRight must traverse all the way ot the end of the list (pushing
    * frames onto the call stack as it goes) before it can begin collapsing it.
    */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

//  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
//    case Nil => z
//    case Cons(h, t) => foldLeft(t, f(z, h))(f)
//  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /**
    * Although we could return [[Nil]] when the input list is empty, we choose to throw an exception instead. This is
    * a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
    * returning a value just means this bug will be discovered later, further from the place where it was introduced.
    * It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
    * right hand side of a pattern. This makes it clear the value isn't relevant.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
    * If a function body consists solely of a match expression, we'll often put the match on the same line as the
    * function signature, rather than introducing another level of nesting.
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * exercise 3.4: Generalize [[tail]] to the function [[drop]], which removes the first n elements from a list.
    */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  /**
    * exercise 3.5: remove elements from the List prefix as long as they match a predicate.
    * Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
    * satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>`
    * can use any of the variables introduced by the pattern.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
    * exercise 3.6: implement a function init, that returns a List consisting of all but the last element of a List
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  /**
    * exercise 3.9: compute the length of a list using foldRight
    */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  /**
    * exercise 3.10: our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for
    * large lists (it's not stack-safe). Write another general list-recursion function, foldLeft, that is tail-recursive
    */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // exercise 3.11: write sum, product, and a function to compute the length of a list using foldLeft
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _)=> acc + 1)

  // exercise 3.12: write a function that returns the reverse of a list using a fold
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  /**
    *  exercise 3.13: (hard) can you write foldLeft in terms of foldRight? How about the other way around? Implementing
    * foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively, which means it works even
    * for large lists without overflowing the stack.
    */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // exercise 3.14: implement append in terms of either foldLeft or foldRight
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))
//    foldRight(l1, l2)((x, l2) => Cons(x, l2))

  /**
    *  exercise 3.15: (hard) write a function that concatenates a list of lists into a single list. Its runtime should be
    * linear in the total length of all lists.

    * Note that we're simply referencing the [[append]] function, without writing something like `(x,y) => append(x,y)`
    * or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are
    * introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other
    * functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
    * In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
    * or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known.

    * Since `append` takes time proportional to its first argument, and this first argument never grows because of the
    * right-associativity of `foldRight`, this function is linear in the total length of all lists.
    */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // exercise 3.16: write a function that transforms a list of integers by adding 1 to each element
  // exercise 3.17: write a function that turns each value in a List[Double] into a String
  // exercise 3.18: write a function map that generalizes modifying each element in a list while maintaining the structure of the list
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  /**
    * A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
    * use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
    * implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
    * mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
    */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map_1[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
    * exercise 3.19: write a function filter that removes elements from a list unless they satisfy a given predicate.
    * The discussion about `map` also applies here.
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  /**
    * exercise 3.20: write a function flatMap that works like map except that the function given will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    */
  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // exercise 3.21: use flatMap to implement filter
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  /**
    * exercise 3.22: write a function that accepts two lists and constructs a new list by adding corresponding elements
    *
    * To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
    * syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
    * objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
    * right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all
    * the variables introduced in the outer `match`.
    * The discussion about stack usage from the explanation of `map` also applies here.
    */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  /**
    * exercise 3.23: generalize the function you just wrote so that it's not specific to integers or addition. Name your
    * generalized function zipWith
    * By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
    */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
   * exercise 3.24: (Hard) implement hasSubsequence for checking whether a List contains another List as a subsequence
   */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}