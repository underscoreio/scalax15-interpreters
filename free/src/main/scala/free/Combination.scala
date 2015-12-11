package free

import cats.free.Free
import cats.{~>,Id}
import cats.data.Xor
import cats.std.list._
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.syntax.xor._

object Combination {
  type Combination[A] = Auth.Auth[A] Xor Orchestration.Request[A]
  type Result[A] = Xor[String, A]
  type UserId = Int

  object LiftLeft extends (Auth.Auth ~> Combination) {
    def apply[A](auth: Auth.Auth[A]): Combination[A] =
      auth.left
  }
  object LiftRight extends (Orchestration.Request ~> Combination) {
    def apply[A](request: Orchestration.Request[A]): Combination[A] =
      request.right
  }

  implicit class LiftLeftOps[A](in: Free[Auth.Auth, A]) {
    def lift: Free[Combination, A] =
      in.compile(LiftLeft)
  }
  implicit class LiftRightOps[A](in: Free[Orchestration.Request, A]) {
    def lift: Free[Combination, A] =
      in.compile(LiftRight)
  }

  object XorInterpreter extends (Combination ~> Result) {
    def apply[A](in: Combination[A]): Result[A] =
      in.fold(
        (auth: Auth.Auth[A]) => Auth.XorInterpreter(auth),
        (request: Orchestration.Request[A]) => Orchestration.IdInterpreter(request).right
      )
  }

  object Example {
    import Orchestration.{Tweet,GetTweets,GetUserName,GetUserPhoto,User}
    import Orchestration.Request._
    import Auth.Credentials
    import Auth.Auth._

    val theId: UserId = 1

    def getUser(creds: Credentials, id: UserId): Free[Combination, User] =
      for {
        _    <- authorize(creds, Auth.Read).lift
        user <- stage(fetch(GetUserName(id)) |@| fetch(GetUserPhoto(id)) map {
                 (name, photo) => User(id, name, photo)
                }).lift
      } yield user

    val result: Free[Combination, List[(String,User)]] =
        for {
          creds  <- authenticate(theId, "password").lift 
          _      <- authorize(creds, Auth.Read).lift
          tweets <- stage(fetch(GetTweets(theId))).lift
          result <- ((tweets map { tweet: Tweet =>
                        for {
                          user <- getUser(creds, tweet.userId)
                        } yield (tweet.msg -> user)
                      }).sequenceU)
        } yield result

    def run: Result[List[(String, User)]] =
      result.foldMap(XorInterpreter)
  }
}
