package u03.cats

import u03.cats.ArithmeticLib.Expression.{Addition, Division, Literal, Multiplication, Subtraction}

import scala.annotation.tailrec

object ArithmeticLib:
  enum Expression:
    case Literal(n: Double)
    case Addition(left: Expression, right: Expression)
    case Subtraction(left: Expression, right: Expression)
    case Multiplication(left: Expression, right: Expression)
    case Division(left: Expression, right: Expression)

    def +(e2: Expression): Expression = Addition(this, e2)
    def -(e2: Expression): Expression = Subtraction(this, e2)
    def *(e2: Expression): Expression = Multiplication(this, e2)
    def /(e2: Expression): Expression = Division(this, e2)

    def eval: Double =
      type Continuation = Double => Call
      enum Call:
        case Continue(value: Double, cont: Continuation)
        case Loop(expr: Expression, cont: Continuation)
        case Done(result: Double)

      import Call.*
      def loop(expr: Expression, cont: Continuation): Call =
        expr match {
          case Literal(n) => Continue(n, cont)
          case Addition(left, right) =>
            Loop(left, l => Loop(right, r => Continue(l + r, cont)))
          case Subtraction(left, right) =>
            Loop(left, l => Loop(right, r => Continue(l - r, cont)))
          case Multiplication(left, right) =>
            Loop(left, l => Loop(right, r => Continue(l * r, cont)))
          case Division(left, right) =>
            Loop(left, l => Loop(right, r => Continue(l / r, cont)))
        }

      @tailrec
      def trampoline(next: Call): Double =
        next match
          case Continue(value, cont) => trampoline(cont(value))
          case Loop(expr, cont)      => trampoline(loop(expr, cont))
          case Done(result)          => result

      trampoline(loop(this, n => Done(n)))

  object Expression:
    def apply(d: Double): Expression = Literal(d)

@main def tryArithmeticLib(): Unit =
  import ArithmeticLib.Expression
  val expression = Expression(5) * (Expression(3) + Expression(2))
  println(expression.eval)