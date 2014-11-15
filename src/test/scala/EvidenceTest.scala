/**
 * Created by mm on 15/03/2014.
 *
 * Tests for section 21.8 Evidence
 *
 */

import org.junit.Test
import org.junit.Assert._


class EvidenceTest {

  @Test
  def testSmallerColonSmallerBasic = {

    def doSth[A,B](sth: A)(implicit evidence: A <:< B):String = sth.toString

    val str = doSth[String, AnyRef]("111")
    println(str)
    assertEquals("checking <:< relation between types String and AnyRef did not work","111", str)

    /**
     * Note - the following won't compile as
     * String does not have a "<:<" relation with Int, i.e., "is subtype of"
     */
    //doSth[String, Int]("222")

  }

  @Test
  def testSmallerColonSmallerBasicImplicitly = {

    assertTrue(implicitly[String <:< AnyRef].isInstanceOf[Function1[String,AnyRef]])

  }

  @Test
  def testEqualsColonEqualsBasic = {

    def doSth[A,B](sth: A)(implicit evidence: A =:= B):String = sth.toString

    val str = doSth[String,String]("111")
    println(str)
    assertEquals("checking =:= relation between types String and String did not work","111",str)

    /**
     * Note - the following won't compile as
     * String does not have a "=:=" relation with AnyRef, i.e., "is equal to"
     */
    //doSth[String, AnyRef]("111")

  }

}
