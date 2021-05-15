package fpinscala.chapter2

import fpinscala.chapter2.ExercisesInChapter2.{fib, isSorted}

object RunJobsCh2 {
  def main(args: Array[String]): Unit = {
    println("*******************")
    println("answer to exercise 2.1:")
    for (i <- 0 to 7) println(fib(i))

    println("*******************")
    println("answer to exercise 2.2:")
    val isIntOrdered =
      (num1: Int, num2: Int) => num1 <= num2
    val isStrOrdered = (char1: String, char2: String) => char1 <= char2
    val intArray = Array(1, 2, 2, 2, 3, 6, 9, 90)
    val strArray = Array("ab", "ab", "abc", "d", "c")
    val res1 = isSorted(intArray, isIntOrdered)
    val res2 = isSorted(strArray, isStrOrdered)
    println(s"Is the Int array ordered? $res1")
    println(s"Is the Str array ordered? $res2")
  }
}
