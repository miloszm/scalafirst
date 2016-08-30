package com.mimu.neophyte.actor

import akka.actor.{ActorRef, ActorSystem, Props}

import scala.concurrent.Future

/**
  * from neophyte's guide to scala
  */
object ActorDemo {

  sealed trait CoffeeRequest

  case object CappussinoRequest extends CoffeeRequest

  case object EspressoRequest extends CoffeeRequest

  case class Bill(cents: Int)

  case object ClosingTime

  object Barista extends App {
    val system = ActorSystem("Barista")

    val barista: ActorRef = system.actorOf(Props[Barista], "Barista")
    val customer: ActorRef = system.actorOf(Props(classOf[Customer], barista), "Customer")

    customer ! CaffeineWithdrawalWarning
    barista ! ClosingTime

    system.shutdown
  }

  import akka.actor.Actor

  class Barista extends Actor {
    var capCount = 0
    var espCount = 0

    def receive = {
      case CappussinoRequest =>
        sender ! Bill(250)
        capCount += 1
        println(s"Got cappuccino request #$capCount")
      case EspressoRequest =>
        sender ! Bill(200)
        espCount += 1
        println(s"Got espresso request #$espCount")
      case ClosingTime =>
        context.system.shutdown
    }
  }


  case object CaffeineWithdrawalWarning

  class Customer(caffeineSource: ActorRef) extends Actor {
    def receive = {
      case CaffeineWithdrawalWarning => caffeineSource ! EspressoRequest
      case Bill(cents) => println(s"paying $cents")
    }
  }


  import akka.pattern.ask
  import akka.util.Timeout
  import scala.concurrent.duration._


  object QuestionsAsker extends App {
    val system = ActorSystem("Barista")
    val barista: ActorRef = system.actorOf(Props[Barista], "Barista")

    implicit val timeout = Timeout(2.second)
    implicit val ec = system.dispatcher

    val f: Future[Any] = barista ? CappussinoRequest
    f.onSuccess {
      case Bill(cents) => println(s"will pay $cents cents")
    }

    println(barista.path)
  }

}