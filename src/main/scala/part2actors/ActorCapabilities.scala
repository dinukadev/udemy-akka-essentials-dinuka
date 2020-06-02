package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => sender() ! "Hello, there!"
      case message: String => println(s"[${self}] I have received $message")
      case number: Int => println(s"[simple actor] I have received a number $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something special : $contents")
      //self ! content means it sends a message back to the same actor which matches the first case
      case SendMessageToYourself(content) => self ! content
      case SayHiTo(ref) => ref ! "Hi!"
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s")
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"


  simpleActor ! 42

  //messages must be immutable
  //messages must be serializable
  //in practice use case classes and case objects
  case class SpecialMessage(contents: String)

  simpleActor ! SpecialMessage("some special content")

  case class SendMessageToYourself(content: String)

  simpleActor ! SendMessageToYourself("I am an actor")


  // actors can reply to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)

  alice ! SayHiTo(bob)

  //here as the sender is null, Akka wills end it to an actor called dead letters
  alice ! "Hi!"

  //forwarding messages

  case class WirelessPhoneMessage(content: String, ref: ActorRef)

  alice ! WirelessPhoneMessage("Hi", bob)

}
