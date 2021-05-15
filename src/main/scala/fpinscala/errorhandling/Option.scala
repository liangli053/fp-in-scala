package fpinscala.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
//    this.map(Some(_)).getOrElse(ob)

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
   * exercise 4.2: implement the [[variance]] function in terms of [[flatMap]]. If the mean of a sequence is m, the
   * variance is the mean of math.pow(x - m, 2)
   */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
   * exercise 4.3: write a generic function [[map2]] that combines two Option values using a binary function. If neither
   * Option values is None, then the return values is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /**
   * since lifting functions is so common in Scala, Scala provides a syntactic construct called the for-comprehension
   * that it expands automatically to a series of flatMap and map calls.
   * The compiler desugars the bindings to flatMap calls, with the final binding and yield being converted to a call
   * to map.
   */
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
  } yield f(aa, bb)

  /**
   * exercise 4.4: write a function [[sequence]] that combines a list of Options into one Option containing a list of
   * all Some values in the original list. If the original list contains None even once, the result of the function
   * should be None; otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  /**
   * It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
   * Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
   * unfortunate consequence of Scala using subtyping to encode algebraic data types.
   */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  /**
   * exerciese 4.5: implement [[traverse]] that only looks at the list onece
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
