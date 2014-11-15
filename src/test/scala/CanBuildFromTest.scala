/**
 * Created by mm on 15/11/2014.
 *
 * Tests for section 21.10 CanBuildFrom Demystified
 *
 *
 */

import org.junit.Test
import org.junit.Assert._

import scala.collection.generic.{CanBuildFrom, GenTraversableFactory}
import scala.collection.immutable.BitSet
import scala.collection.mutable.Builder
import scala.collection.mutable.Buffer
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

    val i2 = implicitly[scala.collection.generic.CanBuildFrom[Set[_], Int, Set[Int]]]

    println(i2)

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

  /**
   * Section 21.10 CanBuildFrom Demystified (SftI)
   *
   * Each collection provides an implicit CanBuildFrom object in its companion object.
   */
  @Test
  def basicCanBuildFromTest = {

    val i1 = implicitly[scala.collection.generic.CanBuildFrom[ArrayBuffer[Int], Int, ArrayBuffer[Int]]]

    println(i1)

    val i2 = implicitly[scala.collection.generic.CanBuildFrom[Buffer[Int], Int, Buffer[Int]]]

    println(i2)

    /**
     * we obtain a builder factory of a Buffer
     */
    val bf:CanBuildFrom[Buffer[Int], Int, Buffer[Int]] = Buffer.canBuildFrom[Int]

    /**
     * we obtain a builder from builder factory
     */
    val b:Builder[Int, Buffer[Int]] = bf.apply

    println(b)

    /**
     * the builder is ArrayBuffer is this case
     * class ArrayBuffer[A] ... with Builder[A, ArrayBuffer[A]]
     * implements +=, clear, result, mapResult
     */
    assertTrue(b.isInstanceOf[ArrayBuffer[Int]])

  }


}
