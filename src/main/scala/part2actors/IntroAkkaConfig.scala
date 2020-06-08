package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

  class LogWithActorLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
    * 1 - inline configuration
    */

  val configString =
    """
      | akka {
      |    loglevel = "DEBUG"
      | }
    """.stripMargin

  val config = ConfigFactory.parseString(configString);
  val system = ActorSystem("ConfigurationDemo", ConfigFactory.load(config))

  val logActor = system.actorOf(Props[LogWithActorLogging], "logActor")

  logActor ! "A message"

  /**
    * 2 - With default application.conf under src/main/resources
    */
  val systemWithAppConf = ActorSystem("ConfigWithAppConfFile")
  val defaultConfigActor = systemWithAppConf.actorOf(Props[LogWithActorLogging], "logWithConf")
  defaultConfigActor ! "Actor with conf"


  /**
    * 3 - separate config in the same file
    */
  val specialConfig = ConfigFactory.load().getConfig("dev")
  val specialConfigSystem = ActorSystem("SpecialConfigSystem", specialConfig)
  val defaultSpecialConfigActor = systemWithAppConf.actorOf(Props[LogWithActorLogging], "logWithSpecialConf")
  defaultSpecialConfigActor ! "Actor with special conf"

  /**
    * 4 - separate config in another file
    */
  val separateConfig = ConfigFactory.load("secretFolder/secretConfiguration.conf")
  println(s"separate config log leve : ${separateConfig.getString("akka.loglevel")}")

  /**
    * 5 - different file formats
    * JSON, Properties
    */
  val jsonConfig = ConfigFactory.load("json/jsonConfig.json")
  println(s"My log level : ${jsonConfig.getString("akka.loglevel")}")
  println(s"My log level : ${jsonConfig.getString("prop")}")
}
