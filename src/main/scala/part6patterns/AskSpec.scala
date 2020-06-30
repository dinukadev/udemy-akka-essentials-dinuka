package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import part6patterns.AskSpec.{AuthFailure, AuthManager, Authenticate, PipedAuthManager, RegisterUser}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}
class AskSpec extends TestKit(ActorSystem("AskSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }


  "An authenticator" should {
    "fail to authenticate a non-registered user" in {
      val authManager = system.actorOf(Props[AuthManager])
      authManager ! Authenticate("test", "test")
      expectMsg(AuthFailure("username not found"))
    }

    "fail to authenticate if invalid password" in {
      val authManager = system.actorOf(Props[AuthManager])
      authManager ! RegisterUser("dinuka", "test")
      authManager ! Authenticate("dinuka", "invalid")
      expectMsg(AuthFailure("password incorrect"))
    }
  }

  "An authenticator with piped approach" should {
    "fail to authenticate a non-registered user" in {
      val authManager = system.actorOf(Props[PipedAuthManager])
      authManager ! Authenticate("test", "test")
      expectMsg(AuthFailure("username not found"))
    }

    "fail to authenticate if invalid password" in {
      val authManager = system.actorOf(Props[PipedAuthManager])
      authManager ! RegisterUser("dinuka", "test")
      authManager ! Authenticate("dinuka", "invalid")
      expectMsg(AuthFailure("password incorrect"))
    }
  }

}

object AskSpec {

  case class Read(key: String)

  case class Write(key: String, value: String)

  class KVActor extends Actor with ActorLogging {
    override def receive: Receive = online(Map())

    def online(kv: Map[String, String]): Receive = {
      case Read(key) =>
        log.info(s"Reading key $key")
        sender() ! kv.get(key)

      case Write(key, value) =>
        log.info(s"Writing key : $key with value : $value")
        context.become(online(kv + (key -> value)))
    }
  }

  case class RegisterUser(userName: String, password: String)

  case class Authenticate(userName: String, password: String)

  case class AuthFailure(message: String)

  case object AuthSuccess


  class AuthManager extends Actor with ActorLogging {
    protected val authDB = context.actorOf(Props[KVActor])
    implicit val timeout: Timeout = Timeout(1 second)
    implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = {
      case RegisterUser(userName, password) =>
        authDB ! Write(userName, password)
      case Authenticate(username, password) => handleAuthentication(username, password)

    }

    def handleAuthentication(userName: String, password: String) = {
      val originalSender = sender()
      val future = authDB ? Read(userName)
      future.onComplete {
        case Success(None) => originalSender ! AuthFailure("username not found")
        case Success(Some(dbPassword)) =>
          if (dbPassword == password) originalSender ! AuthSuccess
          else originalSender ! AuthFailure("password incorrect")
        case Failure(_) => originalSender ! AuthFailure("system error")
      }
    }
  }

  class PipedAuthManager extends AuthManager {
    override def handleAuthentication(userName: String, password: String): Unit = {
      val future = authDB ? Read(userName)
      val passwordFuture = future.mapTo[Option[String]]
      val responseFuture = passwordFuture.map {
        case None => AuthFailure("username not found")
        case Some(dbPassword) =>
          if (dbPassword == password) AuthSuccess
          else AuthFailure("password incorrect")
      }
      responseFuture.pipeTo(sender())
    }
  }

}
