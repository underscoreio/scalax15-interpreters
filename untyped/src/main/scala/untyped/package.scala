package object untyped {
  import cats.data.Xor

  /** Represent the result of evaluating an expression. We represent errors as strings */
  type Result[A] = Xor[String, A]
}
