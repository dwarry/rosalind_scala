package uk.org.warry.rosalind

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
    def fib_tail(m: Int, a: Long, b: Long): Long =
      m match {
        case 0 => a
        case _ => fib_tail(m - 1, b, (k * a) + b)
      }

    fib_tail(n, 0, 1)
  }
}
