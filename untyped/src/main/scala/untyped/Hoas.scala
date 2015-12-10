package untyped

import cats.syntax.xor._
import cats.syntax.apply._

object Hoas {

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
  final case class Bool(get: Boolean) extends Value {
    val name = "Bool"
  }
  final case class Function(f: Value => Expression) extends Value {
    val name = "Function"
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
    implicit object boolRefine extends ValueRefinement[Boolean] {
      def apply(in: Value): Result[Boolean] = {
        in match {
          case Bool(b) => b.right
          case v       => (Errors.wrongTag(v, "Bool")).left
        }
      }
    }
    implicit object functionRefine extends ValueRefinement[Value => Expression] {
      def apply(in: Value): Result[Value => Expression] = {
        in match {
          case Function(f) => f.right
          case v           => (Errors.wrongTag(v, "Function")).left
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

    implicit val booleanInject: ValueInjection[Boolean] =
      (b) => Bool(b)
  }

  object Lift {
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._
    import Expression._

    // Lift a function on types A ... to a function on Values. Use this to make
    // Scala's builtins work with Values.

    def lift[A: ValueRefinement, B: ValueInjection](f: A => B): Expression =
      fun { a =>
        Primitive(() => a.refine[A] map (a => f(a).inject[B]))
      }

    def lift[A: ValueRefinement,B: ValueRefinement, C: ValueInjection](f: (A,B) => C): Expression =
      fun { a =>
        fun { b =>
          Primitive(() => (a.refine[A] |@| b.refine[B] map ((a, b) => f(a,b).inject[C])))
        }
      }
  }

  // Expressions ----------

  sealed trait Expression {
    def eval: Result[Value] =
      ???

    def apply(arg: Expression): Expression =
      Apply(this, arg)

    def apply(arg: Value): Expression =
      Apply(this, Literal(arg))
  }
  final case class Literal(get: Value) extends Expression
  final case class Apply(f: Expression, arg: Expression) extends Expression
  final case class If(cond: Expression, thence: Expression, otherwise: Expression) extends Expression
  final case class Primitive(f: () => Result[Value]) extends Expression
  object Expression {

    // Smart constructors ----------

    def literal(in: Value): Expression =
      Literal(in)
    def number(in: Double): Expression =
      Literal(Number(in))
    def chars(in: String): Expression =
      Literal(Chars(in))
    def fun(f: Value => Expression): Expression =
      Literal(Function(f))
    def perhaps(cond: Expression)(thence: Expression)(otherwise: Expression): Expression = 
      If(cond, thence, otherwise)
    val t = Bool(true)
    val f = Bool(false)
  }

  object Environment {
    import Injections._
    import Injection._
    import Refinements._
    import Refinement._
    import Lift._

    val + = lift[Double,Double,Double](_ + _)
    val - = lift[Double,Double,Double](_ - _)
    val * = lift[Double,Double,Double](_ * _)
    val / = lift[Double,Double,Double](_ / _)

    val < = lift[Double,Double,Boolean](_ < _)
    val > = lift[Double,Double,Boolean](_ > _)
    val == = lift[Double,Double,Boolean](_ == _)

    val `number->chars` = lift[Double,String](_.toString)

    val `chars-append` = lift[String,String,String](_ ++ _)
    val `chars-length` = lift[String,Double](_.length.toDouble)
  }

  object Examples {
    import Environment._
    import Expression._

    def square() = {
      val sq = fun(x => *(x)(x))
      val four = sq(number(2))
      val nine = sq(number(3))

      println(s"four is ${four.eval}")
      println(s"nine is ${nine.eval}")
    }

    def conditional() = {
      val four = perhaps(<(number(1))(number(2))){ number(4.0) }{ chars("bogus") }
      val bogus = perhaps(>(number(1))(number(2))){ number(4.0) }{ chars("bogus") }

      println(s"four is ${four.eval}")
      println(s"bogus is ${bogus.eval}")
    }
  }
}
