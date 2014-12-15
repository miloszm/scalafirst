package com.mimu.fpinscala

import org.junit.Assert._
import org.junit.Test

import scala.annotation.tailrec

/**
 * Created by mm.
 */

sealed trait OptionM[+A]{

  /**
   * 4.1
   */
  def map[B](f: A => B): OptionM[B] = this match {
    case s:SomeM[A] => SomeM(f(s.get))
    case NoneM => NoneM
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case s:SomeM[A] => s.get
    case NoneM => default
  }

  def flatMap[B](f: A => OptionM[B]): OptionM[B] = {
    map(f) getOrElse NoneM
  }

  def orElse[B >: A](ob: => OptionM[B]): OptionM[B] = {
    map(SomeM(_)) getOrElse ob
  }

}

case class SomeM[+A](get: A) extends OptionM[A]{
}

case object NoneM extends OptionM[Nothing]




class chapter4 {

  @Test
  def testOptionMap(): Unit ={
    assertEquals(SomeM(6), SomeM(3) map (_*2))
    val niente:OptionM[Int] = NoneM
    assertEquals(NoneM, niente map (_*2))
  }

  @Test
  def testOptionGetOrElse(): Unit ={
    assertEquals(3, SomeM(3).getOrElse(5))
    val niente:OptionM[Int] = NoneM
    assertEquals(5, niente getOrElse (5))
  }

  @Test
  def testFlatMap(): Unit ={
    assertEquals(SomeM(3), SomeM(3).flatMap((x) => if (x == 3) SomeM(3) else NoneM))
    assertEquals(NoneM, SomeM(4).flatMap((x) => if (x == 3) SomeM(3) else NoneM))
    val niente:OptionM[Int] = NoneM
    assertEquals(NoneM, niente flatMap ((x) => if (x == 3) SomeM(3) else NoneM))
  }

  @Test
  def testOrElse(): Unit ={
    assertEquals(SomeM[Int](3), SomeM[Int](3) orElse SomeM[Int](4))
    val niente:OptionM[Int] = NoneM
    assertEquals(SomeM[Int](4), niente orElse SomeM[Int](4))
  }

}
