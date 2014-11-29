package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * Tagged Type
 *
 *
 *
 */
class TaggedTypeTest {

  /**
   *
   * basic tagged type test
   *
   */
  @Test
  def testTaggedType():Unit = {

    sealed trait MmTaintedStringTrait

    type MmTaintedString = String @@ MmTaintedStringTrait

    val v:MmTaintedString = "abc".asInstanceOf[MmTaintedString]

    val u:MmTaintedString = Tag("defghi")

    /**
     * will accept only tagged class and not the underlying class, in this case String
     * the underlying class needs to be "unwrapped" though
     */
    def testTypeAcceptance(a:MmTaintedString): Unit ={
      val l = Tag.unwrap(a).length
      println(s"$a is of length $l")
    }

    testTypeAcceptance(v)
    //testTypeAcceptance("abc") // won't compile which is what we want
    testTypeAcceptance(u)

    println(v.isInstanceOf[MmTaintedString])

  }

  /**
   * from: http://timperrett.com/2012/06/15/unboxed-new-types-within-scalaz7
   * this is really cool
   */
  @Test
  def monoidWithTaggedTypeTest(): Unit ={

    import Tags._

    println(3 |+| 5)
    println(Multiplication(3) |+| Multiplication(5))

    assertEquals(8, 3 |+| 5)
    assertEquals(15, Multiplication(3) |+| Multiplication(5))
  }

  /**
   * eed3si9n kilogram example
   */
  @Test
  def test_eed3si9n_kilogram_example(): Unit ={

    sealed trait KiloGram

    def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)

    type KiloGramT = Int @@ KiloGram

    val mass = KiloGram(20.0)

    val m = Tag.unwrap(mass)

    println(m)

    /**
     * Removes tag
     */
    val um = Tag.unsubst[Double, Id, KiloGram](mass)

    println(um)

    def doSthWithKilogram(a:Int @@ KiloGram): Unit ={

    }

    def doSthWithKilogram2(a:KiloGramT): Unit ={

    }

    //doSthWithKilogram(20) // luckily it does not compile
    doSthWithKilogram(KiloGram(20)) // compiles
    //doSthWithKilogram2(20) // luckily it does not compile
    doSthWithKilogram2(KiloGram(20)) // compiles

  }



}
