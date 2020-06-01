package part1recap

import scala.concurrent.Future

object AdvancedRecap extends App {

  //partial functions
  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  val pf = (x: Int) => x match {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  val function: (Int => Int) = partialFunction

  val modifiedList = List(1, 2, 3).map {
    case 1 => 42
    case _ => 0
  }

  //lifting
  val lifted = partialFunction.lift //transform it into a total function Int=> Option[Int]
  lifted(2) // Some(65)
  lifted(5000) // Returns None

  //orElse
  val pfChain = partialFunction.orElse[Int, Int] {
    case 60 => 900
  }

  pfChain(5) // 999
  pfChain(60) // 9000
  pfChain(457) // throws a MatchError

  //type aliases
  type ReceiveFunction = PartialFunction[Any, Unit]

  def receive: ReceiveFunction = {
    case 1 => println("hello")
    case _ => println("confused")
  }

  //implicits
  implicit val timeout = 3000

  def setTimeout(f: () => Unit)(implicit timeout: Int) = f()

  setTimeout(() => println("timeout")) // extra parameter list omitted as it is defined as an implicit

  //implicit conversions
  // 1. implicit defs
  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def fromStringToPerson(string: String): Person = Person(string)

  "Peter".greet // fromStringToPerson("Peter").greet - automatically by the compiler

  //2. implicit classes
  implicit class Dog(name: String) {
    def bark = println("bark!")
  }

  "Lassie".bark

  //organize implicits
  //local scope
  implicit val inverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  List(1, 2, 3).sorted // List(3,2,1) as implicit used is the one defined above

  //imported scope
  import scala.concurrent.ExecutionContext.Implicits.global // this is the implicit used by the Futures
  val future = Future {
    println("hello, future")
  }

  // companion objects of the types included in the call

  object Person {
    implicit  val personOrdering: Ordering[Person] = Ordering.fromLessThan((a,b)=> a.name.compareTo(b.name) <0)
  }

  List(Person("Bob"), Person("Alice")).sorted
  //
}
