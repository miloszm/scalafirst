package com.mimu.fpinscala.ch4

import com.mimu.fpinscala.{EitherM, LeftM, RightM}

/**
  * 4.2
  */
object VarianceCalculatorEither extends App {

  def mean(xs: Seq[Double]):EitherM[String,Double] = xs match {
    case Nil => LeftM("mean on empty list")
    case _ => RightM(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): EitherM[String,Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def variance2(xs: Seq[Double]): EitherM[String,Double] = {
    for {
      m <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    }
    yield {
      v
    }
  }

  println(variance(List(2.0, 3.0)))
  println(variance2(List(2.0, 3.0)))

}
