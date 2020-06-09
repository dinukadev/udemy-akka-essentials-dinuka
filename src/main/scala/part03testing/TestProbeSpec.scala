package part03testing

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import part03testing.TestProbeSpec._

class TestProbeSpec extends TestKit(ActorSystem("TestProbeSpec"))
  with WordSpecLike
  with ImplicitSender
  with BeforeAndAfterAll {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A master actor" should {
    "register a slave" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
    }
  }

  "send the work to the slave actor" in {
    val master = system.actorOf(Props[Master])
    val slave = TestProbe("slave")
    master ! Register(slave.ref)
    expectMsg(RegistrationAck)

    val workloadString = "I love akka"
    master ! Work(workloadString)

    slave.expectMsg(SlaveWork(workloadString, testActor))
    slave.reply(WorkCompleted(3, testActor))

    expectMsg(Report(3))
  }

  "aggregate data correctly" in {
    val master = system.actorOf(Props[Master])
    val slave = TestProbe("slave")
    master ! Register(slave.ref)
    expectMsg(RegistrationAck)

    val workloadString = "I love akka"
    master ! Work(workloadString)
    master ! Work(workloadString)

    slave.receiveWhile() {
      case SlaveWork(`workloadString`, `testActor`) => slave.reply(WorkCompleted(3, testActor))
    }
    expectMsg(Report(3))
    expectMsg(Report(6))
  }

}

object TestProbeSpec {

  case class Register(slaveRef: ActorRef)

  case class Work(text: String)

  case class SlaveWork(text: String, originalRequester: ActorRef)

  case class WorkCompleted(count: Int, originalRequester: ActorRef)

  case class Report(totalCount: Int)

  case object RegistrationAck

  class Master extends Actor {
    override def receive: Receive = {
      case Register(slaveRef: ActorRef) =>
        sender() ! RegistrationAck
        context.become(online(slaveRef, 0))

      case _ =>
    }

    def online(slaveRef: ActorRef, totalWorkdCount: Int): Receive = {
      case Work(text) => slaveRef ! SlaveWork(text, sender())
      case WorkCompleted(count, originalRequester) =>
        val newTotalWordCount = totalWorkdCount + count
        originalRequester ! Report(newTotalWordCount)
        context.become(online(slaveRef, newTotalWordCount))
    }
  }

}
