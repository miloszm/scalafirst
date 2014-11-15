/**
 * Created by mm on 15/03/2014.
 *
 * Tests for section 21.7 Context Bounds
 *
 */

import com.mimu.MiloszPair
import org.junit.Test
import org.junit.Assert.assertEquals


class MiloszPairTest {

  @Test
  def testMiloszPairWithInts = {
    val mp = MiloszPair(3,4)
    println(mp.smaller)
    assertEquals(3,mp.smaller())
  }


  @Test
  def testMiloszPairWithZombies = {

    case class Zombie(val z:Int)

    implicit object ZombieOrdering extends Ordering[Zombie] {
      override def compare(x: Zombie, y: Zombie): Int = x.z.compare(y.z)
    }

    val mp = MiloszPair(Zombie(3),Zombie(4))
    println(mp.smaller)
    assertEquals(Zombie(3),mp.smaller())

  }

}
