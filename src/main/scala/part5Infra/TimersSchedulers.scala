package part5Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}

import scala.concurrent.duration._
object TimersSchedulers extends App {

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimersDemo")
  val simpleActor = system.actorOf(Props[SimpleActor],"simpleActor")

  system.log.info("Scheduling reminder for simple actor")

  system.scheduler.scheduleOnce(1 second){
    simpleActor ! "reminder"
  }(system.dispatcher)
}
