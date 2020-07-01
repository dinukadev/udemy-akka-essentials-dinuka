package part6patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import part6patterns.FSMSpec._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class FSMSpec extends TestKit(ActorSystem("FSMSPec"))
  with WordSpecLike
  with ImplicitSender
  with BeforeAndAfterAll {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A vending machine" should {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError("Not initialized"))
    }
  }

  "report a product not available" in {
    val vendingMachine = system.actorOf(Props[VendingMachine])
    vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))
    vendingMachine ! RequestProduct("sandwich")
    expectMsg(VendingError("Product unavailable"))
  }

  "throw a timeout if I do not insert money" in {
    val vendingMachine = system.actorOf(Props[VendingMachine])
    vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))

    vendingMachine ! RequestProduct("coke")
    expectMsg(Instruction("Please insert 1 dollars"))

    within(1.5 seconds) {
      expectMsg(VendingError("TimedOut"))
    }

  }
}

object FSMSpec {

  case class Initialize(inventory: Map[String, Int], prices: Map[String, Int])

  case class RequestProduct(product: String)


  case class Instruction(instruction: String)

  case class ReceiveMoney(amount: Int)

  case class Deliver(product: String)

  case class GiveBackChange(amount: Int)

  case class VendingError(reason: String)

  case object ReceiveMoneyTimeout

  class VendingMachine extends Actor with ActorLogging {
    override def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) => context.become(operational(inventory, prices))
      case _ => sender() ! VendingError("Not initialized")
    }

    def operational(inventory: Map[String, Int], prices: Map[String, Int]): Receive = {
      case RequestProduct(product) => inventory.get(product) match {
        case None | Some(0) =>
          sender() ! VendingError("Product unavailable")

        case Some(_) =>
          val price = prices(product)
          sender() ! Instruction(s"Please insert $price dollars")
          context.become(waitForMoney(inventory, prices, product, price, startReceiveMoneyTimeoutSchedule, sender()))
      }
    }

    def waitForMoney(inventory: Map[String, Int],
                     prices: Map[String, Int],
                     product: String,
                     money: Int,
                     moneyTimeoutSchedule: Cancellable,
                     requester: ActorRef): Receive = {
      case ReceiveMoneyTimeout =>
        requester ! VendingError("TimedOut")
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
    }

    implicit val executionContext: ExecutionContext = context.dispatcher

    def startReceiveMoneyTimeoutSchedule = context.system.scheduler.scheduleOnce(1 second) {
      self ! ReceiveMoneyTimeout
    }
  }

}
