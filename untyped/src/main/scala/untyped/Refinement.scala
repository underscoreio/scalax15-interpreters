package untyped

import cats.data.Xor

object Refinement {
  type Refinement[I,O] = I => Result[O]

  implicit class RefinementOps[I](in: I) {
    def refine[O](implicit r: Refinement[I,O]): Result[O] =
      r(in)
  }
}
