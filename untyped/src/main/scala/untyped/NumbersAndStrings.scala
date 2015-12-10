package untyped

import cats.syntax.xor._
import cats.syntax.apply._

object NumbersAndStrings {

  // Values and refinements of values ----------

  sealed trait Value {
    def name: String
  }
  final case class Number(get: Double) extends Value {
    val name = "Number"
  }
  final case class Chars(get: String) extends Value {
    val name = "Chars"
  }

  object Errors {
    def wrongTag(value: Value, expected: String): String =
      s"Expected value with tag $expected but received value $value with tag ${value.name}"
  }

  object Refinements {
    import Refinement._
    type ValueRefinement[A] = Refinement[Value,A]

    implicit object doubleRefine extends ValueRefinement[Double] {
      def apply(in: Value): Result[Double] = {
        in match {
          case Number(d) => d.right
          case v         => (Errors.wrongTag(v, "Number")).left
        }
      }
    }
    implicit object stringRefine extends ValueRefinement[String] {
      def apply(in: Value): Result[String] = {
        in match {
          case Chars(s) => s.right
          case v        => (Errors.wrongTag(v, "Chars")).left
        }
      }
    }
  }

  object Injections {
    import Injection._
    type ValueInjection[A] = Injection[A,Value]

    implicit val doubleInject: ValueInjection[Double] =
      (d) => Number(d)

    implicit val stringInject: ValueInjection[String] =
      (s) => Chars(s)
  }

  object Lift {
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._

    // Lift a function on types A ... to a function on Values. Use this to make
    // Scala's builtins work with Values.

    def lift[A: ValueRefinement, B: ValueInjection](f: A => B): Value => Result[Value] =
      (v: Value) => v.refine[A] map (a => f(a).inject[B])

    def lift[A: ValueRefinement,B: ValueRefinement, C: ValueInjection](f: (A,B) => C): (Value, Value) => Result[Value] =
      (v1: Value, v2: Value) => (v1.refine[A] |@| v2.refine[B]) map ((a, b) => f(a,b).inject[C])
  }

  // Expressions ----------

  sealed trait Expression {
    def eval: Result[Value] =
      ???

    def +(that: Expression): Expression =
      Plus(this, that)
    def -(that: Expression): Expression =
      Minus(this, that)
    def *(that: Expression): Expression =
      Multiply(this, that)
    def /(that: Expression): Expression =
      Divide(this, that)
    def ++(that: Expression): Expression =
      StringAppend(this, that)
    def toChars: Expression =
      NumberToChars(this)
    def toUpper: Expression =
      ToUpper(this)
    def toLower: Expression =
      ToLower(this)
  }
  object Expression {
    // Smart constructors that creates Value with type Expression ----------

    def number(in: Double): Expression =
      Literal(Number(in))
    def chars(in: String): Expression =
      Literal(Chars(in))
  }
  final case class Literal(value: Value) extends Expression
  final case class Plus(left: Expression, right: Expression) extends Expression
  final case class Minus(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression) extends Expression
  final case class Divide(left: Expression, right: Expression) extends Expression
  final case class StringAppend(left: Expression, right: Expression) extends Expression
  final case class NumberToChars(expr: Expression) extends Expression
  final case class ToChars(expr: Expression) extends Expression
  final case class ToUpper(expr: Expression) extends Expression
  final case class ToLower(expr: Expression) extends Expression

  object Example {
    def go() = {
      import Expression._

      val expr1 = number(1.0) + number(3.0) / number(2.0)
      val expr2 = chars("Hello, ") ++ chars("world!")
      val expr3 = (number(1.0) * number(4.0)).toUpper

      println(expr1.eval)
      println(expr2.eval)
      println(expr3.eval)
    }
  }
}
