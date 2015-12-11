package free

import cats.free.Free
import cats.{~>,Id}
import cats.data.Xor
import cats.syntax.xor._

object Auth {
  type UserId = Int
  type Result[A] = String Xor A

  final case class Credentials(secretKey: String)
  sealed trait Action
  final case object Read extends Action
  final case object Write extends Action

  sealed trait Auth[A]
  final case class Authenticate(user: UserId, password: String) extends Auth[Credentials]
  final case class Authorize(credentials: Credentials, action: Action) extends Auth[Unit]

  object Auth {
    def authenticate(userId: UserId, password: String): Free[Auth, Credentials] =
      Free.liftF(Authenticate(userId, password))

    def authorize(credentials: Credentials, action: Action): Free[Auth, Unit] =
      Free.liftF(Authorize(credentials, action))
  }

  object XorInterpreter extends (Auth ~> Result) {
    import Id._

    def apply[A](in: Auth[A]): Result[A] =
      in match {
        case Authenticate(userId, password) =>
          println(s"Authenticating $userId")
          userId match {
            case 1 => Credentials("AgnesSecret").right
            case 2 => Credentials("BrianSecret").right
            case _ => s"User $userId did not successfully authenticate".left
          }

        case Authorize(credentials, action) =>
          println(s"Authorizing $credentials for $action")
          (credentials, action) match {
            case (Credentials("AgnesSecret"), _)    => ().right
            case (Credentials("BrianSecret"), Read) => ().right
            case _ =>
              s"$credentials is not authorized to perform $action".left
          }
      }
  }
}
