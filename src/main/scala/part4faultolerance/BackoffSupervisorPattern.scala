package part4faultolerance

import java.io.File

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, Props}
import akka.pattern.{Backoff, BackoffSupervisor}

import scala.concurrent.duration._
import scala.io.Source

object BackoffSupervisorPattern extends App {

  case class ReadFile(file: String)

  class FileBasedPersistentActor extends Actor with ActorLogging {
    var dataSource: Source = null

    override def preStart(): Unit = log.info("Starting")

    override def postStop(): Unit = log.warning("Persistent actor has stopped")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.warning("Persistent actor restarting")

    override def receive: Receive = {
      case ReadFile(file:String) =>
        if (dataSource == null)
          dataSource = Source.fromFile(new File(file))
        log.info("I have read some imortant data " + dataSource.getLines().toList)
    }
  }

  val system = ActorSystem("BackoffSupervisorDemo")
  val simpleActor = system.actorOf(Props[FileBasedPersistentActor], "simpleActor")
  simpleActor ! ReadFile("src/main/resources/testfiles/important.txt")

  val simpleSupervisorProps = BackoffSupervisor.props(
    Backoff.onFailure(
      Props[FileBasedPersistentActor],
      "simpleBackoffActor",
      3 seconds,
      30 seconds,
      0.2
    )
  )

  val simpleBackoffSupervisor = system.actorOf(simpleSupervisorProps, "simpleSupervisor")
 simpleBackoffSupervisor ! ReadFile("src/main/resources/testfiles/important_invalid.txt")

  val stopSupervisorProps = BackoffSupervisor.props(
    Backoff.onStop(Props[FileBasedPersistentActor],
      "stopBackoffActor",
      3 seconds,
      30 seconds,
      0.2
    ).withSupervisorStrategy(
      OneForOneStrategy() {
        case _ => Stop
      }
    )
  )

  val simpleStopSupervisor = system.actorOf(stopSupervisorProps,"stopSupervisor")
  simpleStopSupervisor ! ReadFile("src/main/resources/testfiles/important_invalid.txt")

  class EagerFileBasedPersistentActor extends FileBasedPersistentActor {
    override def preStart(): Unit = {
      log.info("Eager actor starting")
      dataSource = Source.fromFile(new File("src/main/resources/testfiles/important_invalid.txt"))
    }
  }

  val eagerActor = system.actorOf(Props[EagerFileBasedPersistentActor],"eagerActor")

  val repeatedSupervisorProps = BackoffSupervisor.props(
    Backoff.onStop(
      Props[EagerFileBasedPersistentActor],
      "eagerSuperActor",
      1 seconds,
      30 seconds,
      0.1
    )
  )

  val repeatedActor = system.actorOf(repeatedSupervisorProps,"repeatedSupActor")

}
