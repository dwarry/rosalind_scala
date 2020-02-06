package uk.org.warry.rosalind

import scala.annotation.tailrec
import scala.collection.mutable

object Combinatorics {

  /**
   * Calculates a modified Fibonacci sequence where each generation grows by the factor `k`.
   * The normal Fibonacci series is given by k=1.
   * @param n The number of generations to calculate
   * @param k The growth factor of each generation
   * @return the nth member of the series
   */
  def fib(n: Int, k: Int): Long = {
    /* This illustrates a common approach to avoiding stack overflows in recursive functions:
     * the recursive function is written in tail-recursive form; the recursive call is the
     * very last thing the function does. This enables the compiler to rewrite the call as
     * a loop, or the runtime can  re-use the stackframe rather than allocate a new one each time.
     *
     * If the default case were re-written as `fib_tail(m - 1, b, (k * a) + b) * 1` it would no
     * longer be a tail call as it would need to return to do the multiplication.
     *
     * This illustrates a common pattern - the top-level function defines a nested, tail-recursive
     * helper function that does the bulk of the work when called with the initial values provided
     * by the top-level function. This helper function often has extra "state" parameters that
     * contain the intermediate results. This also illustrates the "dynamic programming" nature of
     * the problem - see https://en.wikipedia.org/wiki/Dynamic_programming - a naïve implementation
     * has to recalculate every step
     * e.g.
     *     def naive_fib(n: Int, k: Int): Long = {
             if (n <= 2) 1 else (k * naive_fib(n - 2, k)) + naive_fib(n - 1, k)
           }
     * and is [loosely] in O(2^n) - the number of operations increases exponentially as the number of
     * generations. In contrast, the tail-recursive form is in O(n)
     *
     * In this case, `fib_tail` is a "closure" that captures the value of
     * `k` rather than it being passed in explicitly.
     */
    @tailrec
    def fib_tail(m: Int, a: Long, b: Long): Long =
      m match {
        case 0 => a
        case _ => fib_tail(m - 1, b, (k * a) + b)
      }

    fib_tail(n, 0, 1)
  }

  // https://www.scala-lang.org/api/current/scala/collection/immutable/LazyList.html
  // private val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  /**
   * Calculates the mortal fibonacci sequence. Rabbits only reproduce after the first generation,
   * and only live a set number of generations.
   * @param n number of generations of rabbits
   * @param m number of generations that rabbits live
   * @return the number of rabbits alive after n
   */
  def fibd(n: Int, m:Int): BigInt  = {

    // previously computed values.
    val memo = scala.collection.mutable.HashMap((1, BigInt(1)), (2, BigInt(1)))

    // Does the actual calculation - only calculate the value of the intermediate results once.
    def mortalFib(i: Int): BigInt = {
      i match {
        case x if x <= 0 => 0
        case _ if memo.contains(i) => memo(i)
        case _ =>
          val range = (i - m) to  (i - 2)
          val result = range.map(mortalFib).sum
          memo.addOne((i, result))
          result
      }
    }

    mortalFib(n + 1)
  }

}
