package part6patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, FSM, Props}
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

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please insert 2 dollars"))

      within(1.5 seconds) {
        expectMsg(VendingError("TimedOut"))
        expectMsg(GiveBackChange(1))
      }
    }
    "deliver the product if I insert all the money" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("coke"))
    }

    "give back change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("coke"))
      expectMsg(GiveBackChange(1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))

    }
  }



  "A vending machine FSM" should {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError("Not initialized"))
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

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(Props[VendingMachineFSM])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please insert 2 dollars"))

      within(1.5 seconds) {
        expectMsg(VendingError("TimedOut"))
        expectMsg(GiveBackChange(1))
      }
    }
    "deliver the product if I insert all the money" in {
      val vendingMachine = system.actorOf(Props[VendingMachineFSM])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("coke"))
    }

    "give back change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(Props[VendingMachineFSM])
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))
      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("coke"))
      expectMsg(GiveBackChange(1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars"))

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
          context.become(waitForMoney(inventory, prices, product, 0, startReceiveMoneyTimeoutSchedule, sender()))
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
      case ReceiveMoney(amount) =>
        moneyTimeoutSchedule.cancel()
        val price = prices(product)
        if (money + amount >= price) {
          requester ! Deliver(product)
          if (money + amount - price > 0) requester ! GiveBackChange(money + amount - price)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          context.become(operational(newInventory, prices))
        } else {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"Please insert $remainingMoney dollars")
          context.become(waitForMoney(inventory, prices, product, money + amount,
            startReceiveMoneyTimeoutSchedule, requester))
        }
    }

    implicit val executionContext: ExecutionContext = context.dispatcher

    def startReceiveMoneyTimeoutSchedule = context.system.scheduler.scheduleOnce(1 second) {
      self ! ReceiveMoneyTimeout
    }
  }

  trait VendingState

  case object Idle extends VendingState

  case object Operational extends VendingState

  case object WaitForMoney extends VendingState

  trait VendingData

  case object Uninitialized extends VendingData

  case class Initialized(inventory: Map[String, Int], prices: Map[String, Int]) extends VendingData

  case class WaitForMoneyData(inventory: Map[String, Int],
                              prices: Map[String, Int],
                              product: String,
                              money: Int,
                              requester: ActorRef) extends VendingData

  class VendingMachineFSM extends FSM[VendingState, VendingData] {
    startWith(Idle, Uninitialized)

    when(Idle) {
      case Event(Initialize(inventory, price), Uninitialized) =>
        goto(Operational) using Initialized(inventory, price)
      case _ =>
        sender() ! VendingError("Not initialized")
        stay()
    }
    when(Operational) {
      case Event(RequestProduct(product), Initialized(inventory, prices)) =>
        inventory.get(product) match {
          case None | Some(0) =>
            sender() ! VendingError("Product unavailable")
            stay()
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price dollars")
            goto(WaitForMoney) using WaitForMoneyData(inventory, prices, product, 0, sender())

        }

    }
    when(WaitForMoney, stateTimeout = 1 second) {
      case Event(StateTimeout, WaitForMoneyData(inventory, prices, _, money, requester))=>
        requester ! VendingError("TimedOut")
        if (money > 0) requester ! GiveBackChange(money)
        goto(Operational) using Initialized(inventory,prices)
      case Event(ReceiveMoney(amount), WaitForMoneyData(inventory, prices, product, money, requester)) =>
        val price = prices(product)
        if (money + amount >= price) {
          requester ! Deliver(product)
          if (money + amount - price > 0) requester ! GiveBackChange(money + amount - price)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          goto(Operational) using Initialized(newInventory, prices)
        } else {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"Please insert $remainingMoney dollars")
          stay() using WaitForMoneyData(inventory, prices, product, money + amount, requester)
        }
    }

    whenUnhandled {
      case Event(__, _) =>
        sender() ! VendingError("CommandNotFound")
        stay()
    }

    onTransition {
      case stateA -> stateB => log.info(s"Transitioning from $stateA to $stateB")
    }

    initialize()
  }

}




