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
class WriterMonadTest {

  /**
   * basic Writer test
   */
  @Test
  def testWriterMonad():Unit = {

    val v = 3.set("Smalish gang")

    println(v.getClass)

    val v2:Writer[String,Int] = 3.set("Smalish gang")

    for(e <- v2) println(e)

    println(MonadTell[Writer, String].point(3))

    val v3 = MonadTell[Writer, String].writer("hi monad tell writer", 4)

    println(v3)

    for(e <- v3) println(e)
  }

  /**
   * do notation for writer
   * from: http://eed3si9n.com/learning-scalaz/Writer.html
   */
  @Test
  def testDoNotationForWriter(): Unit = {

    def logNumber(x: Int): Writer[List[String], Int] = x.set(List("Got number: " + x.shows))

    def multWithLog: Writer[List[String], Int] = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b

    println(multWithLog run)

  }

  /**
   * gcd with logging (from Learn you a Haskell & eed3si9n.com
   */
  @Test
  def testGcdWithLogging(): Unit = {

    def gcd(a: Int, b: Int): Writer[Vector[String], Int] =
      if (b == 0) for {
        _ <- Vector("Finished with " + a.shows).tell
      } yield a
      else for {
        result <- gcd(b, a % b)
        _ <- Vector(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell
      } yield result

    println(gcd(24,9))

    assertEquals(3, gcd(24,9).run._2)

  }


}
