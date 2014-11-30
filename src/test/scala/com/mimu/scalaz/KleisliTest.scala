package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * from: http://eed3si9n.com/learning-scalaz/Composing+monadic+functions.html
 *
 * from: Learn you a Haskell
 * <=< function is just like composition, only instead of working for normal functions like a -> b,
 * it works for monadic functions like a -> m b
 *
 */
class KleisliTest {

  /**
   * kleisli
   */
  @Test
  def basicKleisliTest():Unit = {

    val f = Kleisli { (x: Int) => (x + 1).some }
    val g = Kleisli { (x: Int) => (x * 100).some }
    println(4.some >>= (f <=< g))
    println(4.some >>= (f >=> g))

  }

}
