package part4faultolerance

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Kill, PoisonPill, Props, Terminated}
import part4faultolerance.StartingStoppingActors.Parent.{StartChild, Stop, StopChild}

object StartingStoppingActors extends App {

  val system = ActorSystem("StoppingActorsDemo")


  object Parent {

    case class StartChild(name: String)

    case class StopChild(name: String)

    case object Stop

  }

  class Parent extends Actor with ActorLogging {
    override def receive: Receive = withChildren(Map())

    def withChildren(children: Map[String, ActorRef]): Receive = {
      case StartChild(name) =>
        log.info(s"Starting child $name")
        context.become(withChildren(children + (name -> context.actorOf(Props[Child], name))))
      case StopChild(name) =>
        log.info(s"Stopping child with the name $name")
        val childOption = children.get(name)
        //context.stop is non-blocking
        childOption.foreach(childRef => context.stop(childRef))

      case Stop =>
        log.info("Stopping myself")
        //this also stops the child actors as well
        context.stop(self)

    }
  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
    * method 1 - using context.stop
    */
  val parent = system.actorOf(Props[Parent], "parent")

  parent ! StartChild("childOne")

  val child = system.actorSelection("/user/parent/childOne")

  child ! "hi kid!"

  parent ! StopChild("childOne")

  /**
    * method 2 - using PoisonPill message
    */
  val looseActor = system.actorOf(Props[Child])
  looseActor ! "hello, loose actor"
  looseActor ! PoisonPill
  looseActor ! "lose actor, are you still there"

  /**
    * method 3 - using Kill message
    */

  val terminateActor = system.actorOf(Props[Child])
  terminateActor ! "hello, terminateA actor"
  terminateActor ! Kill
  terminateActor ! "terminate actor, are you still there"

  /**
    * Death watch - notifies when an actor dies
    */

  class Watcher extends Actor with ActorLogging {
    override def receive: Receive = {
      case StartChild(name) =>
        val child = context.actorOf(Props[Child], name)
        log.info(s"Started and watching child $name")
        context.watch(child)
      case Terminated(ref) =>
        log.info(s"The reference that I'm watching  $ref has been stopped")
    }
  }

}
