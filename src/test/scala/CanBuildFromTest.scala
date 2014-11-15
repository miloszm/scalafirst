/**
 * Created by mm on 15/11/2014.
 *
 * Tests for section 21.10 CanBuildFrom Demystified
 *
 *
 */

import org.junit.Test
import org.junit.Assert._

import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

class CanBuildFromTest {

  /**
   * Section 25.2 Factoring out common operations (PiS)
   *
   * Implicit resolution provides correct static types for tricky collection operations such as map.
   */
  @Test
  def testBasicCanBuildFrom = {

    val i1 = implicitly[scala.collection.generic.CanBuildFrom[BitSet, Int, BitSet]]

    println(i1)

    assertTrue(i1.isInstanceOf[scala.collection.generic.BitSetFactory])

    val i2 = implicitly[scala.collection.generic.CanBuildFrom[Set[_], Int, Set[Int]]]

    println(i2)

    assertTrue(i1.isInstanceOf[scala.collection.generic.GenSetFactory])
  }

  /**
   * Section 25.1 Builders (PiS)
   */
  @Test
  def basicBuilderTest = {
    val buf = new ArrayBuffer[Int]
    val builder:scala.collection.mutable.Builder[Int,Array[Int]] = buf mapResult (_.toArray)

    builder += 5
    builder += 7

    val a:Array[Int] = builder.result()

    a.foreach(println)
  }


}
