package com.mimu.fpinscala.ch4

/**
  * 4.2
  */
object VarianceCalculator extends App {

  def mean(xs: Seq[Double]):Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def variance2(xs: Seq[Double]): Option[Double] = {
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
