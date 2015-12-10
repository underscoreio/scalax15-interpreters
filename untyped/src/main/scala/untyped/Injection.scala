package untyped

object Injection {
  type Injection[I,O] = I => O

  implicit class InjectionOps[I](in: I) {
    def inject[O](implicit i: Injection[I,O]): O =
      i(in)
  }
}
