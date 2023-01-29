package pp202002.hw1

import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Main
{
    /* Implement given functions, which is currently blank. (???) */

    /*
      **WARNING: Please read the restrictions below carefully.**

      If you do not follow it, **your submission will not be graded.**

      1. Do not use keyword `var`. Use `val` and `def` instead.
      2. Do not use any library functions or data structures like `List`, `Array` `Range` (`1 to n`, `1 until n` ...),
         `fold`, `map`, `reduce` or etc... You can only use tuples, `scala.annotation.tailrec`, and
         `scala.util.control.TailCalls._` from the library.

      Again, if you do not follow these rules, your score will be zero.

      For all three problems, 50% of the test cases will require tail call optimizations (ie, have large inputs)
      and the other 50% will not (ie, have small inputs).

      So, we will get 50% of the score if you submit a correct program without tail call optimization.
     */

    /*
     Exercise 1: Collatz sequence

     Consider this function: for a positive integer n,
     f(n) = n / 2       (if n is even)
     f(n) = 3 * n + 1   (if n is odd)

     Collatz sequence is defined as follows:
     c_0 = n  (n is positive integer)
     c_k = f(c_(k-1))

     Collatz conjecture(https://en.wikipedia.org/wiki/Collatz_conjecture) is the famous open problem,
     that no matter what value of starting value 'n', the sequence will always reach 1.

     Given a number n (1 <= n <= 10^12), compute the length and the sum of collatz sequence
     starting with n, until it reaches 1.

     e.g.) collatz(5) == (6, 36)
           (5 -> 16 -> 8 -> 4 -> 2 -> 1)
     */
    def collatz(n: Long): (Long, Long) =
    {
        @tailrec
        def computeCollatz(n: Long, prev: Long, prevSum:Long): (Long, Long) =
        {
            if (n == 1) (prev + 1, prevSum + 1)
            else if (n % 2 == 0) computeCollatz(n / 2, prev + 1, prevSum + n)
            else computeCollatz(3 * n + 1, prev + 1, prevSum + n)
        }
        computeCollatz(n, 0, 0)
    }


    /*
     Exercise 2: Definite integral by Riemann sum

     Implement definite integral:
     Given a continuous function f, and range [a, b], compute the definite integral value over the interval [a, b]

     The value should be calculated by Riemann sum. (https://terms.naver.com/entry.nhn?docId=2073839&cid=47324&categoryId=47324)
     Divide the interval [a, b] into n equal intervals, and gather the areas below function f through each interval.

     The difference between your result and the real integral value should be less than 0.001

     Hint: Double `n` until the value moves less then 0.001

     Addition 10/04: Assume that the difference with a Riemann sum and a real integral value will always be less than 0.001 when n >= 10^7
     */
    def integral(f: Double => Double, a: Double, b: Double): Double =
    {
        @tailrec
        def calculateSum(f: Double => Double, a: Double, b: Double, n: Int, k: Int, tempSum: Double): Double =
        {
            if (k == 1) f((b - a) / n + a) + tempSum
            else calculateSum(f, a, b, n, k - 1, tempSum + f(((b - a) * k) / n + a))
        }

        @tailrec
        def intIter(f: Double => Double, a: Double, b: Double, n: Int): Double =
        {
            val x = ((b - a) / n) * calculateSum(f, a, b, n, n, 0.0)
            val y = ((b - a)/ (2 * n)) * calculateSum(f, a, b, 2 * n, 2 * n, 0.0)
            if (x - y < 0.0005 && y - x < 0.0005 || n > 10000000) x
            else intIter(f, a, b, 2 * n)
        }

        intIter(f, a, b, 1000)
    }

    /*
     Exercise 3: Ping-pong Ackermann function

     Given a function p and positive integer a and b, implement this function `ppa`: (0 <= a, b <= 10^6)
     ppa(p, a, b) = p(a, b)               (if a <= 0 or b <= 0)
     ppa(p, a, b) = p(a, ppa(p, a-1, b))  (if p(a, b) is even and a, b > 0)
     ppa(p, a, b) = p(ppa(p, a, b-1), b)  (if p(a, b) is odd and a, b > 0)


     Hint 1: If a, b > 0, wrap remained calculations, make it as an anonymous function and pass it to the parameter recursively.
             That anonymous function is called 'Continuation'. You'd better search 'Continuation Passing Style'.

     Hint 2: Due to the limits of Scala's platform (JVM), tail call (not tail recursion) is not optimized generally,
             especially when using continuation passing style.
             To optimize tail call properly, use `tailcall`, `done`, `result` functions from scala.util.control.TailCalls._
             (See https://stackoverflow.com/questions/16539488/why-scala-doesnt-make-tail-call-optimization)
     */
    def ppa(p: (Int, Int) => Int, a: Int, b: Int): Int =
    {
        @tailrec
        def ppa_cont(a: Int, b: Int, cont: Int => TailRec[Int]): TailRec[Int] =
        {
            if (a <= 0 || b <= 0) cont(p(a, b))
            else if (p(a, b) % 2 == 0) ppa_cont(a - 1, b, (x) => tailcall(cont(p(a, x))))
            else ppa_cont(a, b - 1, (x) => tailcall(cont(p(x, b))))
        }

        def cont(x: Int): TailRec[Int] = done(x)
        ppa_cont(a, b, cont).result
    }
}
