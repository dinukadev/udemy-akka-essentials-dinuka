package part6patterns

import akka.actor.{Actor, ActorLogging, Stash}

object StashDemo extends App {

  case object Open

  case object Close

  case object Read

  case class Write(data: String)


  class ResourceActor extends Actor with ActorLogging with Stash {
    private var innerData: String = ""

    override def receive: Receive = closed

    def closed: Receive = {
      case Open =>
        log.info("Opening resource")
        unstashAll()
        context.become(open)
      case message =>
        log.info(s"Stashing $message")
        stash()
    }

    def open: Receive = {
      case Read =>
        log.info(s"Read $innerData")

      case Write(data) =>
        log.info(s"Write $data")
        innerData = data

      case Close =>
        log.info("Closing")
        unstashAll()
        context.become(closed)

      case message =>
        log.info(s"Stashing $message")
        stash()
    }
  }

}
