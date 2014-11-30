package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 *
 */
class ReaderFunctorTest {

  /**
   * (->) r functor
   */
  @Test
  def testReaderFunctor():Unit = {

    val f = (_: Int) * 5
    val g = (_: Int) + 3
    assertEquals(5*(8+3), (g map f)(8))

    val ff = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
    assertEquals(3*2 + (3+10), ff(3))

    /**
     * monadic behavior of partially applied functions
     *
     * function is considered a value with a context
     *
     * the function monad is also called the reader monad. All the functions read from a common source
     * this is really cool
     */
    val addStuff: Int => Int = for {
      a <- (_: Int) * 2
      b <- (_: Int) + 10
    } yield a + b
    assertEquals(19, addStuff(3))

  }


}
