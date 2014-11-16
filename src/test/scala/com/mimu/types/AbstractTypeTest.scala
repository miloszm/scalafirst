package com.mimu.types

import org.junit.Test

import scala.io.Source

/**
 * Created by mm.
 *
 * 18.12 of SftI - AbstractTypes
 *
 */
class AbstractTypeTest {

  @Test
  def basicAbstractTypeTest():Unit = {

    trait Reader {
      type Contents   // abstract type
      def read(fileName:String): Contents
    }

    class StringReader extends Reader {
      override type Contents = String
      override def read(fileName: String): Contents = Source.fromFile(fileName, "UTF-8").mkString
    }

  }

  @Test
  def basicAbstractTypeTest_TypeParameter():Unit = {

    trait Reader[C] {
      def read(fileName:String): C
    }

    class StringReader extends Reader[String] {
      override def read(fileName: String) = Source.fromFile(fileName, "UTF-8").mkString
    }

  }

  @Test
  def abstractTypeTest_WithTypeBounds():Unit = {

    trait Reader {
      type Contents <: Array[Char]
      def read(fileName:String): Contents
    }

    class StringReader extends Reader {
      /**
       * does not compile as String is not <: Array[Char]
       */
      //override type Contents = String
      /**
       * compiles as Array[Char] <: Array[Char]
       */
      override type Contents = Array[Char]
      override def read(fileName: String): Contents = new Array[Char](3)
    }

  }

}
