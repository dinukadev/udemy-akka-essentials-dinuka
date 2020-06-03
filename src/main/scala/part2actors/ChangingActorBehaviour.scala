package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehaviour.FussyKid.{KidAccept, KidReject}
import part2actors.ChangingActorBehaviour.Mom.{Ask, Food, MomStart}

object ChangingActorBehaviour extends App {


  object FussyKid {

    case object KidAccept

    case object KidReject

    val HAPPY = "happy"
    val SAD = "sad"
  }

  class FussyKid extends Actor {
    var state = FussyKid.HAPPY

    override def receive: Receive = {
      case Food(Mom.VEGETABLE) => state = FussyKid.SAD
      case Food(Mom.CHOCOLATE) => state = FussyKid.HAPPY
      case Ask(_) =>
        if (state == FussyKid.HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor{
    override def receive: Receive = happyReceive

    def happyReceive: Receive= {
      case Food(Mom.VEGETABLE) => context.become(sadReceived)
      case Food(Mom.CHOCOLATE) =>
      case Ask(_) => KidAccept
    }

    def sadReceived: Receive ={
      case Food(Mom.VEGETABLE) =>
      case Food(Mom.CHOCOLATE) => context.become(happyReceive)
      case Ask(_) => KidReject
    }
  }

  object Mom {

    case class MomStart(kidRef: ActorRef)

    case class Food(food: String)

    case class Ask(message: String)

    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }

  class Mom extends Actor {
    override def receive: Receive = {
      case MomStart(kidRef) =>
        kidRef ! Food(Mom.VEGETABLE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay, my kid is happy!")
      case KidReject => println("My kid is sad!")
    }
  }

  val system = ActorSystem("ChangingActorBehaviour")
  val kid = system.actorOf(Props[FussyKid], "fussyKid")
  val statelessKid = system.actorOf(Props[FussyKid], "statelessFussyKid")
  val mom = system.actorOf(Props[Mom], "mom")

  mom ! MomStart(statelessKid)
}
