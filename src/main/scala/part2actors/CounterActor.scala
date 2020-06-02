package part2actors

import akka.actor.{Actor, ActorSystem, Props}
import part2actors.CounterActor.Counter.{Decrement, Increment, Print}

object CounterActor extends App {

  val system = ActorSystem("counterSystem")


  object Counter {

    case object Increment

    case object Decrement

    case object Print

  }

  class Counter extends Actor {
    var count: Int = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] My current counter $count")
    }
  }

  val counter = system.actorOf(Props[Counter], "myCounter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)

  counter ! Print

}
