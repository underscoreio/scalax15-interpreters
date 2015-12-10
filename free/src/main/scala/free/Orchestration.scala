package free

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await,Future}
import scala.concurrent.duration.Duration

import cats.free.{Free, FreeApplicative}
import cats.{~>,Id}
import cats.std.future._
import cats.std.list._
import cats.syntax.apply._
import cats.syntax.traverse._

object Orchestration {
  type UserId = Int
  type UserName = String
  type UserPhoto = String

  final case class Tweet(userId: UserId, msg: String)
  final case class User(id: UserId, name: UserName, photo: UserPhoto)

  // Services represent web services we can call to fetch data
  sealed trait Service[A]
  final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
  final case class GetUserName(userId: UserId) extends Service[UserName] 
  final case class GetUserPhoto(userId: UserId) extends Service[UserPhoto]

  // A request represents a request for data
  sealed trait Request[A]
  final case class Pure[A](get: A) extends Request[A]
  final case class Fetch[A](service: Service[A]) extends Request[A]
  final case class Stage[A](request: FreeApplicative[Request,A]) extends Request[A]


  object Request {
    def stage[A](stage: FreeApplicative[Request, A]): Free[Request, A] =
      Free.liftF(Stage(stage))

    def pure[A](a: A): FreeApplicative[Request, A] =
      FreeApplicative.lift(Pure(a) : Request[A])

    def fetch[A](service: Service[A]): FreeApplicative[Request, A] =
      FreeApplicative.lift(Fetch(service) : Request[A])
  }

  object ToyInterpreter extends (Request ~> Id) {
    import Id._

    def apply[A](in: Request[A]): Id[A] =
      in match {
        case Pure(a) => a
        case Stage(r) => r.foldMap(ToyInterpreter)
        case Fetch(service) =>
          service match {
            case GetTweets(userId) =>
              println(s"Getting tweets for user $userId")
              List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))

            case GetUserName(userId) =>
              println(s"Getting user name for user $userId")
              userId match {
                case 1 => "Agnes"
                case 2 => "Brian"
                case _ => "Anonymous"
              }

            case GetUserPhoto(userId) =>
              println(s"Getting user photo for user $userId")
              userId match {
                case 1 => ":-)"
                case 2 => ":-D"
                case _ => ":-|"
              }
          }
      }
  }

  object FutureInterpreter extends (Request ~> Future) {
    import Id._

    def apply[A](in: Request[A]): Future[A] =
      in match {
        case Pure(a) => Future.successful(a)
        case Stage(r) => r.foldMap(FutureInterpreter)
        case Fetch(service) =>
          service match {
            case GetTweets(userId) =>
              Future {
                println(s"Getting tweets for user $userId")
                List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))
              }

            case GetUserName(userId) =>
              Future {
                println(s"Getting user name for user $userId")
                  userId match {
                    case 1 => "Agnes"
                    case 2 => "Brian"
                    case _ => "Anonymous"
                  }
              }

            case GetUserPhoto(userId) =>
              Future {
                println(s"Getting user photo for user $userId")
                userId match {
                  case 1 => ":-)"
                  case 2 => ":-D"
                  case _ => ":-|"
                }
              }
          }
      }
  }

  object Example {
    import Request._

    val theId: UserId = 1

    def getUser(id: UserId): FreeApplicative[Request, User] =
      (fetch(GetUserName(id)) |@| fetch(GetUserPhoto(id))) map {
        (name, photo) => User(id, name, photo)
      }

    val result: Free[Request, List[(String,User)]] =
      for {
        tweets <- stage(fetch(GetTweets(theId)))
        result <- stage((tweets map { tweet: Tweet =>
                     for {
                       user <- getUser(tweet.userId)
                     } yield (tweet.msg -> user)
                   }).sequenceU)
      } yield result

    def run: List[(String, User)] =
      Await.result(result.foldMap(FutureInterpreter), Duration.Inf)
  }
}
