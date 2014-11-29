package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 *
 */
class BreakingTheLawTest {

  /**
   * test broken functor
   */
  @Test
  def testFunctorWithOption():Unit = {

    implicit def coptionEqual[A]: Equal[COption[A]] = Equal.equalA

    implicit val coptionFunctor = new Functor[COption] {
      def map[A, B](fa: COption[A])(f: A => B): COption[B] = fa match {
        case CNone => CNone
        case CSome(c, a) => CSome(c + 1, f(a))
      }
    }

    println((CSome(0, "ho"): COption[String]) map {(_: String) + "ha"})

    println((CSome(0, "ho"): COption[String]) map {identity})


  }

  /**
   * check broken laws
   */
  @Test
  def checkBrokenLaws():Unit = {
  }


}


sealed trait COption[+A] {}

case class CSome[A](counter: Int, a: A) extends COption[A]

case object CNone extends COption[Nothing]

