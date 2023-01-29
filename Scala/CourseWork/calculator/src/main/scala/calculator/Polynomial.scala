package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Signal {
          b() * b() - 4 * a() * c()
      }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      Signal {
          val s: Set[Double] = Set()
          val d = delta()
          val root = Math.sqrt(d)
          val x1 = (-1 * b() + root) / (2 * a())
          val x2 = (-1 * b() - root) / (2 * a())
          if (!x1.isNaN) {
              if (!x2.isNaN) {
                  ((s + x1) + x2)
              } else {
                  s + x1
              }
          } else {
              if (!x2.isNaN) {
                  s + x2
              } else {
                  s
              }
          }
      }
  }
