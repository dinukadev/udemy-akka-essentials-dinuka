package part5Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.dispatch.{ControlMessage, PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.{Config, ConfigFactory}

object MailBoxes extends App {

  val system = ActorSystem("MailBoxDemo", ConfigFactory.load().getConfig("mailboxesDemo"))

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
    * #1 - custom priority mailbox
    */

  class SupportTicketPriorityMailbox(settings: ActorSystem.Settings, config: Config)
    extends UnboundedPriorityMailbox(
      PriorityGenerator {
        case message: String if message.startsWith("[P0]") => 0
        case message: String if message.startsWith("[P1]") => 1
        case message: String if message.startsWith("[P2]") => 2
        case message: String if message.startsWith("[P3]") => 3
        case _ => 4

      })

  val supportTicketLogger = system.actorOf(Props[SimpleActor].withDispatcher("support-ticket-dispatcher"))
  supportTicketLogger ! "[P3] this is a p3"
  supportTicketLogger ! "[P0] this is a p0"
  supportTicketLogger ! "[P1] this is a p1"


  /**
    *  #2 - control-aware mailbox using UnboundedContextAwareMailBox
    */
  case object ManagementTicket extends ControlMessage

  val controlAwareActor = system.actorOf(Props[SimpleActor].withMailbox("control-mailbox"))

  /**
    * #3 - using deployment config
    */

  val altControlAwareActor = system.actorOf(Props[SimpleActor],"altControlAwareActor")

}
