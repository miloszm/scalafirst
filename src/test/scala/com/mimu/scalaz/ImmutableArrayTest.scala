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
class ImmutableArrayTest {

  /**
   * immutable array
   */
  @Test
  def testImmutableArray():Unit = {

    val xs = ImmutableArray.fromArray(Array("test"))
    val t = xs.tail
    assertEquals(Nil, t)

  }

}
