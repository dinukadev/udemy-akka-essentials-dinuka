package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.BankActor.BankAccount.{Deposit, Statement, TransactionFailure, TransactionSuccess, Withdraw}
import part2actors.BankActor.Person.LiveTheLife


object BankActor extends App {

  val system = ActorSystem("bankAccountSystem")

  object BankAccount {

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object Statement

    case class TransactionSuccess(message: String)

    case class TransactionFailure(reason: String)

  }

  class BankAccount extends Actor {
    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount < 0) sender() ! TransactionFailure("invalid deposit amount")
        else {
          funds += amount
          sender() ! TransactionSuccess("successfully deposited amount")
        }

      case Withdraw(amount) => {
        if (amount < 0) sender() ! TransactionFailure("invalid withdraw amount")
        else if (amount > funds) sender() ! TransactionFailure("insufficient funds")
        else {
          funds -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }
      }
      case Statement => sender() ! s"Your balance is $funds"

    }
  }

  object Person {

    case class LiveTheLife(account: ActorRef)

  }

  class Person extends Actor {
    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(900000)
        account ! Withdraw(500)
        account ! Statement

      case message => println(message.toString)
    }
  }

  val account = system.actorOf(Props[BankAccount], "bankAccount")
  val person = system.actorOf(Props[Person],"person")


  person ! LiveTheLife(account)
}
