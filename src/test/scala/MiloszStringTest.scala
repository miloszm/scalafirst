/**
 * Created by mm on 15/03/2014.
 *
 * Tests for sections 21.2 Using Implicits for Enriching Existing Libraries
 *                    21.3 Importing Implicits
 *                    21.5 Implicit Parameters
 *
 */

import org.junit.Test
import org.junit.Assert.assertEquals
import com.mimu.Delimiters
import com.mimu.MiloszString
import com.mimu.MiloszString.{d => _,_}

class MiloszStringTest {

  @Test
  def testMiloszStringCountMs() = {
    val str = "aamaaMaam";
    println(str.countMs)
    assertEquals(3,str.countMs)
  }

  @Test
  def testMiloszStringWithDelims_Explicit() = {
    val str = MiloszString("aaa")
    println(str.withDelims(Delimiters("<<<", ">>>")))
  }

  @Test
  def testMiloszStringWithDelims_Implicit_Local() = {
    val str = MiloszString("aaa")
    implicit val d = Delimiters("<<<", ">>>")
    println(str.withDelims)
  }

  @Test
  def testMiloszStringWithDelims_Implicit_Imported() = {
    val str = MiloszString("aaa")
    import com.mimu.MiloszString.d
    println(str.withDelims)
  }

}
