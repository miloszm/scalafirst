package com.mimu.implicits

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 */
class ImplicitClassTest {

  @Test
  def testImplicitClass(): Unit = {
    assertEquals("aaaaaa", "aaa".someMethod())
  }

}
