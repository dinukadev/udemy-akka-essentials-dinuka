package part03testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import part03testing.SynchronousTestingSpec.{Counter, Increment, Read}

import scala.concurrent.duration.Duration

class SynchronousTestingSpec extends WordSpecLike with BeforeAndAfterAll {

  implicit val system = ActorSystem("SynchronousTestingSpec")

  override protected def afterAll(): Unit = {
    system.terminate()
  }

  "A counter" should {
    "synchronously increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Increment

      assert(counter.underlyingActor.count ==1)
    }
  }

  "synchronously increase its counter at the call of the receive function" in {
    val counter = TestActorRef[Counter](Props[Counter])
    counter.receive(Increment)
    assert(counter.underlyingActor.count ==1)
  }

  "worked on the calling thread dispatcher" in {
    val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
    val probe = TestProbe()
    probe.send(counter, Read)
    probe.expectMsg(Duration.Zero,0)
  }

}

object SynchronousTestingSpec {

  case object Increment

  case object Read

  class Counter extends Actor {
    var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Read => sender() ! count
    }
  }

}
