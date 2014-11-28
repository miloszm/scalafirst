package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import std.list._

/**
 * Created by mm.
 *
 *
 */
class FunctorLiftTest {

  /**
   * simple lift test
   */
  @Test
  def testLift():Unit = {

    val liftedFun = Functor[List].lift {(_: Int) * 3}

    println(liftedFun)

    val l = liftedFun(List(1,2,3,4))

    println(l)

    assertEquals(List(3,6,9,12),l)

  }

  /**
   * other functor operations
   */
  @Test
  def testOtherFunctorOperations(): Unit = {

    import Scalaz._


    assertEquals(List("m", "m", "m"), List(1, 2, 3) >| "m")

    assertEquals(List("m", "m", "m"), List(1, 2, 3) as "m")

    assertEquals(List((15,15), (25,25), (35,35)), List(15, 25, 35).fpair)

    assertEquals(List(("m",1), ("m",2), ("m",3)), List(1, 2, 3).strengthL("m"))

    assertEquals(List((1, "m"), (2, "m"), (3, "m")), List(1, 2, 3).strengthR("m"))

    assertEquals(List((), (), ()), List(1, 2, 3).void)

  }

}
