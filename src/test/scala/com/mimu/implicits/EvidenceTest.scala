package com.mimu.implicits

/**
 * Created by mm on 15/03/2014.
 *
 * Tests for section 21.8 Evidence
 *                   21.9 The @implicitNotFound Annotation
 *
 */

import org.junit.Assert._
import org.junit.Test

import scala.annotation.implicitNotFound


class EvidenceTest {

  @Test
  def testSmallerColonSmallerBasic = {

    def doSth[A,B](sth: A)(implicit evidence: A <:< B):String = sth.toString

    val str = doSth[String, AnyRef]("111")
    println(str)
    assertEquals("checking <:< relation between types String and AnyRef did not work","111", str)

  }

  @Test
  def testSmallerColonSmallerBasic_ImplicitNotFoundAnnotation = {

    @implicitNotFound(msg = "Can't prove that ${A} is subtype of ${B}")
    def doSth[A,B](sth: A)(implicit evidence: A <:< B):String = sth.toString

    /**
     * Note - the following won't compile as
     * String does not have a "<:<" relation with Int, i.e., "is subtype of"
     * Compilation error will be as defined in the @implicitNotFound annotation
     * To see it uncomment out this code and try to compile
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

  @Test
  def testEqualsColonEqualsBasicImplicitly = {

    assertTrue(implicitly[String =:= String].isInstanceOf[Function1[String,String]])

  }

}
