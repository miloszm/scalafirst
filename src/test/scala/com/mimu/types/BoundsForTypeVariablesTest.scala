package com.mimu.types

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 *
 * Tests for section 17.3 SftI - Bounds for Type Variables
 *
 */
class BoundsForTypeVariablesTest {

  @Test
  def testUpperBound():Unit = {

    class Pair[T <: Comparable[T]](val first:T, val second:T){
      def smaller = if (first.compareTo(second) < 0) first else second
    }

    val p1 = new Pair[String]("a","&")
    println(p1.smaller)
    assertEquals("&",p1.smaller)

    /**
     * the following line gives compilation error
     */
    //val p2 = new Pair[Int](3,5)
    //println(p2 smaller)

  }

  @Test
  def testViewBound():Unit = {

    class Pair[T <% Comparable[T]](val first:T, val second:T){
      def smaller = if (first.compareTo(second) < 0) first else second
    }

    val p1 = new Pair[String]("a","&")
    println(p1.smaller)
    assertEquals("&",p1.smaller)

    /**
     * the following line works for Ints
     * A <% B means - A can be converted to B through an implicit conversion
     */
    val p2 = new Pair[Int](3,5)
    println(p2 smaller)
    assertEquals(3,p2.smaller)

  }

  /**
   * T <% Ordered[T] preferable to T <% Comparable[T]
   */
  @Test
  def testViewBoundOrdered():Unit = {

    class Pair[T <% Ordered[T]](val first:T, val second:T){
      def smaller = if (first < second) first else second
    }

    val p1 = new Pair[String]("a","&")
    assertEquals("&",p1.smaller)
    val p2 = new Pair[Int](3,5)
    assertEquals(3,p2.smaller)

  }

  /**
   * lower bound
   *
   * B >: A - means any class B which is a supertype of A
   *
   */
  @Test
  def testLowerBound():Unit = {
    class Pair[A](val first:A, val second:A) {
      def replaceSecond[B >: A](newSecond: B) = new Pair[B](first, newSecond)
      override def toString() = "(" + first.getClass.getSimpleName + "," + second.getClass.getSimpleName + ")"
    }

    class Dad
    class Son extends Dad

    val p1 = new Pair[Son](new Son, new Son) // will return pair of Son
    val p2 = p1.replaceSecond(new Dad) // will return pair of Dad

    println(p2)
    assertTrue(p1.isInstanceOf[Pair[Son]]) // this does not really check the 'Son' part
    assertTrue(p2.isInstanceOf[Pair[Dad]]) // this does not really check the 'Dad' part
  }

}
