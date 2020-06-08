package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActors.Parent.{CreateChild, TellChild}

object ChildActors extends App {

  object Parent {

    case class CreateChild(name: String)

    case class TellChild(message: String)

  }

  class Parent extends Actor {

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))

    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) =>
        childRef forward message
    }
  }


  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} i got : $message")
    }
  }

  val system = ActorSystem("childActor")

  val parentActor = system.actorOf(Props[Parent],"parent")
  parentActor ! CreateChild("child")
  parentActor ! TellChild("hey kid")


}
