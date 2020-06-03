package part2actors

import akka.actor.{Actor, ActorSystem, Props}
import part2actors.CounterActorWithBehaviour.CounterActor.{Decrement, Increment, Print}

object CounterActorWithBehaviour extends App {

  object CounterActor {

    case object Increment

    case object Decrement

    case object Print

  }

  class CountActor extends Actor {
    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment => context.become(countReceive(currentCount + 1))
      case Decrement => context.become(countReceive(currentCount - 1))
      case Print => println(s"Current count is : $currentCount")
    }
  }

  val system = ActorSystem("countActorWithBehaviour")
  val countActor = system.actorOf(Props[CountActor])

  countActor ! Increment
  countActor ! Increment
  countActor ! Decrement
  countActor ! Print

}
