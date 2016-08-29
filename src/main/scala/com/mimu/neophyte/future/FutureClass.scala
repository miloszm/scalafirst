package com.mimu.neophyte.future

import scala.concurrent.Future
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * from neophyte's guide to scala
  */
object FutureClass extends App {

  type CoffeeBeans = String
  type GroundCoffee = String
  case class Water(temperature: Int)
  type Milk = String
  type FrothedMilk = String
  type Espresso = String
  type Cappuccino = String

  case class GrindingException(msg: String) extends Exception(msg)
  case class FrothingException(msg: String) extends Exception(msg)
  case class WaterBoilingException(msg: String) extends Exception(msg)
  case class BrewingException(msg: String) extends Exception(msg)

  def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
    println("start grinding...")
    Thread.sleep(Random.nextInt(2000))
    if (beans == "baked beans") throw GrindingException("are you joking?")
    println ("finished grinding...")
    s"ground coffee of $beans"
  }

  def heatWater(water: Water): Future[Water] = Future {
    println("heating the water now")
    Thread.sleep(Random.nextInt(2000))
    println("hot, it's hot!")
    water.copy(temperature = 85)
  }

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    println("milk frothing system engaged!")
    Thread.sleep(Random.nextInt(2000))
    println("shutting down milk frothing system")
    s"frothed $milk"
  }

  def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
    println("happy brewing :)")
    Thread.sleep(Random.nextInt(2000))
    println("it's brewed!")
    "espresso"
  }

  def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"

  val p = grind("arabica")
  val pp = p.onSuccess{case x:GroundCoffee => println(s" ready $x")}

  val r = grind("baked beans")
  r.onComplete {
    case Success(ground) => println(s"ok $ground")
    case Failure(ex) => println(s"exception ${ex.getClass}")
  }

  val temperatureOkay2: Future[Unit] = heatWater(Water(20)).map {
    water => println(s"water ok ${water.temperature}")
  }

  //temperatureOkay2

  def temperatureOkay(water:Water):Future[Boolean] = Future {
    (80 to 85).contains(water.temperature)
  }

  val f = heatWater(Water(20)).flatMap{ w => temperatureOkay(w)}

  f.onComplete {
    case Success(s) => println(s"f ok ${s.getClass}")
    case Failure(ex) => println(s"exception ${ex.getClass}")
  }

  val ff = for {
    water <- heatWater(Water(20))
    okay <- temperatureOkay(water)
  } yield okay

  ff.onComplete {
    case Success(s) => println(s"ff ok ${s.getClass}")
    case Failure(ex) => println(s"exception ${ex.getClass}")
  }

  def prepareCappuccino(): Future[Cappuccino] = {
    val g = grind("arabica")
    val h = heatWater(Water(22))
    val f = frothMilk("milk") // these features must be instantated befor for comprehension to make computation really parallel
    for {
      gg <- g
      hh <- h
      ff <- f
      espresso <- brew (gg, hh)
    }
    yield combine(espresso, ff)
  }


  prepareCappuccino().onComplete {
    case Success(s) => println(s"cappuccino ok ${s.getClass} $s")
    case Failure(ex) => println(s"cappuccino exception ${ex.getClass}")
  }


  Thread.sleep(4000)

}
