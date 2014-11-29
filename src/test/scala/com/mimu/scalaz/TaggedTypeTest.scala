package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * Tagged Type
 *
 *
 *
 */
class TaggedTypeTest {

  /**
   *
   * basic tagged type test
   *
   */
  @Test
  def testTaggedType():Unit = {

    sealed trait MmTaintedStringTrait

    type MmTaintedString = String @@ MmTaintedStringTrait

    val v:MmTaintedString = "abc".asInstanceOf[MmTaintedString]

    val u:MmTaintedString = Tag("defghi")

    /**
     * will accept only tagged class and not the underlying class, in this case String
     * the underlying class needs to be "unwrapped" though
     */
    def testTypeAcceptance(a:MmTaintedString): Unit ={
      val l = Tag.unwrap(a).length
      println(s"$a is of length $l")
    }

    testTypeAcceptance(v)
    //testTypeAcceptance("abc") // won't compile which is what we want
    testTypeAcceptance(u)

    println(v.isInstanceOf[MmTaintedString])

  }



}
