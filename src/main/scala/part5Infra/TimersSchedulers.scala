package part5Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props}

import scala.concurrent.duration._

object TimersSchedulers extends App {

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimersDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  system.log.info("Scheduling reminder for simple actor")

  implicit val executionContext = system.dispatcher
  system.scheduler.scheduleOnce(1 second) {
    simpleActor ! "reminder"
  }

  val routine: Cancellable = system.scheduler.schedule(
    1 second,
    2 seconds){
    simpleActor ! "heartbeat"
  }

  system.scheduler.scheduleOnce(
    5 seconds
  ){
    routine.cancel()
  }

}
