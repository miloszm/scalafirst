package com.mimu.implicits

/**
 * Created by mm.
 *
 * Tests for section 21.7 Context Bounds
 *
 */

import org.junit.Assert.assertEquals
import org.junit.Test


class MimuPairTest {

  @Test
  def testMiloszPairWithInts = {
    val mp = MimuPair(3,4)
    println(mp.smaller)
    assertEquals(3,mp.smaller())
  }


  @Test
  def testMiloszPairWithZombies = {

    case class Zombie(val z:Int)

    implicit object ZombieOrdering extends Ordering[Zombie] {
      override def compare(x: Zombie, y: Zombie): Int = x.z.compare(y.z)
    }

    val mp = MimuPair(Zombie(3),Zombie(4))
    println(mp.smaller)
    assertEquals(Zombie(3),mp.smaller())

  }

}
