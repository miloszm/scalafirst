package com.mimu.fpinscala.ch6

import com.mimu.fpinscala.StateM
import com.mimu.fpinscala.StateM.sequence
import com.mimu.fpinscala.StateM.modify
import com.mimu.fpinscala.StateM.get

/**
  * 6.11
  */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked:Boolean, candies:Int, coins:Int) {

    def simulateMachine(inputs: List[Input]): StateM[Machine, List[(Int, Int)]] = {

      val l = for {
        i <- inputs
      }
      yield {
        i match {
          case Coin => coin
          case Turn => turn
        }
      }

      val x = sequence(l)
      x

    }

    def coin:StateM[Machine, (Int, Int)] = StateM(m => {
      if (m.candies > 0){
        ((m.coins+1, m.candies), Machine(false, m.candies, m.coins+1))
      }
      else {
        ((m.coins, m.candies), Machine(m.locked, m.candies, m.coins))
      }
    })

    def turn:StateM[Machine, (Int, Int)] = StateM(m => {
      if (m.candies > 0 && !m.locked){
        ((m.coins, m.candies-1), Machine(true, m.candies-1, m.coins))
      }
      else {
        ((m.coins, m.candies), Machine(m.locked, m.candies, m.coins))
      }
    })

  }

  object Candy {

    def update: (Input => (Machine) => Machine) = (i: Input) => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }

    def simulateMachine(inputs: List[Input]): StateM[Machine, (Int,Int)] = for {
      _ <- sequence(inputs map update map modify)
      s <- get
    } yield (s.coins, s.candies)

  }

object CandyDispenser extends App {


  val f = Machine(true, 10, 0).simulateMachine(List(Coin,Turn,Coin,Turn))
  println(f.run(Machine(true, 10, 0)))

  val ff = Candy.simulateMachine(List(Coin,Turn,Coin,Turn))
  println(ff.run(Machine(true, 10, 0)))

}
