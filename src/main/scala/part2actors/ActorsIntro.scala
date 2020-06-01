package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  class WordCountActor extends Actor {
    //internal data
    var totalWords = 0

    //behaviour
    def receive: Receive = {
      case message: String =>
        println(s"[word counter] I have received : $message")
        totalWords += message.split(" ").length
      case msg => println((s"[word counter] I cannot understand ${msg.toString}"))
    }
  }

  //instantiate an actor. Cannot instantiate with new
  //good practice to name your actors
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")

  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  //exclamation mark is the method we are invoking. Exclamation method is also known as "tell"
  //sending this message is asynchronous
  wordCounter ! "I am learning Akka"
  anotherWordCounter ! "A different message"


  object Person {
    def props(name: String)= Props(new Person(name))
  }
  class Person(name:String) extends Actor{
    override def receive: Receive = {
      case "hi" => println(s"Hi my name is $name")
      case _ =>
    }
  }

  // instantiating an actor class. note that this way is discouraged
  val person = actorSystem.actorOf(Props(new Person("Bob")))
  person ! "hi"

  //this is the best practice. to have a companion object create a prop with the object you want
  val personRightWay = actorSystem.actorOf(Person.props("Bob"))




}
