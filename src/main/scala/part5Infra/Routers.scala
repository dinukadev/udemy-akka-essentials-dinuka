package part5Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing._
import com.typesafe.config.ConfigFactory

object Routers extends App {

  /**
    * Method 1 - Manual router
    */

  class Master extends Actor {
    private val slaves = for (i <- 1 to 5) yield {
      val slave = context.actorOf(Props[Slave], s"slave_${i}")
      context.watch(slave)
      ActorRefRoutee(slave)
    }
    private var router = Router(RoundRobinRoutingLogic(), slaves)

    override def receive: Receive = {
      case Terminated(ref) =>
        router.removeRoutee(ref)
        val newSlave = context.actorOf(Props[Slave])
        router.addRoutee(newSlave)
      case message =>
        router.route(message, sender())

    }
  }

  class Slave extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("RoutersDemo", ConfigFactory.load().getConfig("routersDemo"))
  val master = system.actorOf(Props[Master], "masterActor")

    for (i <- 1 to 10) {
      master ! s"[$i] Hello from the world"
    }

  /**
    * Method 2 - Pool router
    */

  val poolMaster = system.actorOf(RoundRobinPool(5).props(Props[Slave]), "simplePoolMaster")
    for (i <- 1 to 10) {
      poolMaster ! s"[$i] Hello from the world"
    }

  /**
    * Method 3 from configuration
    */

  val poolMaster2 = system.actorOf(FromConfig.props(Props[Slave]), "poolMaster2") //name should match what is defined in the application.conf

    for (i <- 1 to 10) {
      poolMaster2 ! s"[$i] Hello from the world"
    }

  /**
    * Method 4 -  GROUP router
    */

  val slaveList = (1 to 5).map(i => system.actorOf(Props[Slave], s"slave_${i}")).toList

  val slavePaths = slaveList.map(slaveRef => slaveRef.path.toString)

  val groupMaster = system.actorOf(RoundRobinGroup(slavePaths).props())


  for (i <- 1 to 10) {
    groupMaster ! s"[$i] Hello from the world"
  }

  /**
    * Method 4 - Group router with configuration
    */

  val groupMasterWithConfig = system.actorOf(FromConfig.props(),"groupMaster2")
    for (i <- 1 to 10) {
      groupMasterWithConfig ! s"[$i] Hello from the world"
    }
}

