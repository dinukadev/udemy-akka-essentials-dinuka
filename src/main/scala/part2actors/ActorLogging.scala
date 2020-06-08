package part2actors


import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLogging extends App {

  class SimpleLoggingActor extends Actor {
    val logger = Logging(context.system, this)

    override def receive: Receive = {
      case message => logger.info(message.toString)
    }
  }

  val system = ActorSystem("Logging")
  val actor = system.actorOf(Props[SimpleLoggingActor], "simpleLoggingActor")
  actor ! "Log message"

  class LogWithActorLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => log.info("Two params: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  val actorLogging = system.actorOf(Props[LogWithActorLogging], "logWithActorLogging")
  actorLogging ! "Log with actor logging"
  actorLogging ! ("first param","second param")
}
