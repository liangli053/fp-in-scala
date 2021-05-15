package fpinscala.chapter2

import scala.annotation.tailrec
/**
 * All exercises in Chapter 2 of Functional Programming in Scala
 */
object ExercisesInChapter2 extends App {
  /**
   * exercise 2.1: write a recursive function to ge the nth fibonacci number
   * Your definition should use a local tail-recursive function
   */
  def fib(input: Int) : Int = {
    @tailrec
    def helper(idx: Int, fib1: Int, fib2: Int): Int = {
      if (input < 2) input
      else if (idx == input) fib1 + fib2
      else helper(idx + 1, fib2, fib1 + fib2)
    }

    helper(2, 0, 1)
  }

  /**
   * exercise 2.2: implement isSorted, which checks whether an Array[A] is sorted according
   * to a given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    @tailrec
    def helper(n: Int): Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else helper(n + 1)

    if (as.isEmpty || as.length == 1) true else helper(1)
  }

  /**
   * exercise 2.3: currying -- converts a function f of two arguments into a function of
   * one argument that partially applies f
   * Generally, we omit the type annotation on a function literal if it can be inferred by Scala
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    // "=>" associates to the right, A => (B => C) can be written as A => B => C
//    (a: A) => (b: B) => f(a, b)
    a => b => f(a, b)
  }

  /**
   * exercise 2.4: implement uncurry
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
//    (a: A, b: B) => f(a)(b)
    (a, b) => f(a)(b)

  /**
   * exercise 2.5: implement the higher-order function that composes two functions
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
//    (a: A) => f(g(a))
    a => f(g(a))
}
