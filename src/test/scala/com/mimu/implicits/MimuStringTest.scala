package com.mimu.implicits

/**
 * Created by mm on 15/03/2014.
 *
 * Tests for sections 21.2 Using Implicits for Enriching Existing Libraries
 *                    21.3 Importing Implicits
 *                    21.5 Implicit Parameters
 *
 */

import com.mimu.implicits.MimuString.{d => _, _}
import org.junit.Assert.assertEquals
import org.junit.Test

class MimuStringTest {

  @Test
  def testMiloszStringCountMs() = {
    val str = "aamaaMaam";
    println(str.countMs)
    assertEquals(3,str.countMs)
  }

  @Test
  def testMiloszStringWithDelims_Explicit() = {
    val str = MimuString("aaa")
    println(str.withDelims(Delimiters("<<<", ">>>")))
  }

  @Test
  def testMiloszStringWithDelims_Implicit_Local() = {
    val str = MimuString("aaa")
    implicit val d = Delimiters("<<<", ">>>")
    println(str.withDelims)
  }

  @Test
  def testMiloszStringWithDelims_Implicit_Imported() = {
    val str = MimuString("aaa")
    import com.mimu.implicits.MimuString.d
    println(str.withDelims)
  }

}
