package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      namedExpressions.map((s, e) => (s, Signal{eval(e(), namedExpressions)}))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = {
      def evalIter(expr: Expr, s: Set[String]): Double = {
          expr match {
              case Expr.Literal(v) => v
              case Expr.Ref(name) => {
                  if (s.contains(name))
                      Double.NaN
                  else {
                      val ref = getReferenceExpr(name, references)
                      evalIter(ref, s + name)
                  }
              }
              case Expr.Plus(a, b) => evalIter(a, s) + evalIter(b, s)
              case Expr.Minus(a, b) => evalIter(a, s) - evalIter(b, s)
              case Expr.Times(a, b) => evalIter(a, s) * evalIter(b, s)
              case Expr.Divide(a, b) => evalIter(a, s) / evalIter(b, s)
          }
      }
      evalIter(expr, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
