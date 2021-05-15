package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * exercise 3.25: write a function size that counts the number of nodes (leaves and branches) in a tree
   */
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
   * exercise 3.26: write a function maximum that returns the maximum element in a Tree[Int]
   */
  def maximum(node: Tree[Int]): Int = node match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
   * exercise 3.27: write a function depth that returns the maximum path length from the root of a tree to any leaf
   */
  def depth[A](node: Tree[A]): Int = node match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  /**
   * exercise 3.28: write a function map, analogous to the method of the same name on List, that modifies each element
   * in a tree with a given function
   */
  def map[A, B](node: Tree[A])(f: A => B): Tree[B] = node match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * exercise 3.29: generalize size, maximum, depth and map, writing a new function [[fold]] that abstracts over their
   * similarities. Reimplement them in terms of this more general function. Can you draw an analogy between this fold
   * function and the left and right fold for List?
   *
   * Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
   * accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
   * this function to implement just about any recursive function that would otherwise be defined by pattern matching.
   */
  def fold[A, B](node: Tree[A])(f: A => B)(g: (B, B) => B): B = node match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
