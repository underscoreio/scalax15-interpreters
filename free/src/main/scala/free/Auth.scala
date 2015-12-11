package free

import cats.free.Free
import cats.{~>,Id}

object Auth {
  type UserId = Int

  final case class Credentials(secretKey: String)
  sealed trait Action
  final case object Read extends Action
  final case object Write extends Action

  sealed trait Auth[A]
  final case class Authenticate(user: UserId, password: String) extends Auth[Option[Credentials]]
  final case class Authorize(credentials: Credentials, action: Action) extends Auth[Boolean]

  object Auth {
    def authenticate(userId: UserId, password: String): Free[Auth, Option[Credentials]] =
      Free.liftF(Authenticate(userId, password))

    def authorize(credentials: Credentials, action: Action): Free[Auth, Boolean] =
      Free.liftF(Authorize(credentials, action))
  }

  object IdInterpreter extends (Auth ~> Id) {
    import Id._

    def apply[A](in: Auth[A]): Id[A] =
      in match {
        case Authenticate(userId, password) =>
          println(s"Authenticating $userId")
          userId match {
            case 1 => Some(Credentials("AgnesSecret"))
            case 2 => Some(Credentials("BrianSecret"))
            case _ => None
          }

        case Authorize(credentials, action) =>
          println(s"Authorizing $credentials for $action")
          (credentials, action) match {
            case (Credentials("AgnesSecret"), _)    => true
            case (Credentials("BrianSecret"), Read) => true
            case _                                  => false
          }
      }
  }
}
