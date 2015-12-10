package untyped

object Arithmetic {
  sealed trait Expression {
    def eval: Double =
      ???

    def +(that: Expression): Expression =
      Plus(this, that)
    def -(that: Expression): Expression =
      Minus(this, that)
    def *(that: Expression): Expression =
      Multiply(this, that)
    def /(that: Expression): Expression =
      Divide(this, that)
  }
  object Expression {
    // Smart constructor that creates Value with type Expression
    def value(in: Double): Expression =
      Value(in)
  }
  final case class Plus(left: Expression, right: Expression) extends Expression
  final case class Minus(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression) extends Expression
  final case class Divide(left: Expression, right: Expression) extends Expression
  final case class Value(get: Double) extends Expression

  object Examples {
    import Expression._

    val four = value(2) + value(2)
    val nine = value(3) * value(3)
    val one  = (value(3) * value(2)) / (value(3) + value(3))

    def go() = {
      println(s"Four evaluates to ${four.eval}")
      println(s"Nine evaluates to ${nine.eval}")
      println(s"One evaluates to ${one.eval}")
    }
  }
}
