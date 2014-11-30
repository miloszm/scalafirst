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
class BigGangTest {

  /**
   * big gang example from Learn you a Haskell and http://eed3si9n.com/learning-scalaz/Writer.html
   */
  @Test
  def testBigGang():Unit = {

    def makeSureBigEnough(x: Int): (Int, String) = {
      if (x > 9){
        (x, s"Compared gang size $x to 9 and it is big enough. ")
      }
      else {
        (x + 2, s"Compared gang size $x to 9 and it is too small, added 2 members so it has ${x+2} now. ")
      }
    }

    def doubleIt(x: Int): (Int, String) = (x+x, s"Doubled it to ${x+x}. ")

    def tripleIt(x: Int): (Int, String) = (x*3, s"Tripled it to ${x*3}. ")

    implicit class PairOps[A](pair: (A, String)) {
      def applyLog[B](f: A => (B, String)): (B, String) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log ++ newlog) // y is f(x) but log is concatenated with old log
      }
    }

    println((3, "Smallish gang. ") applyLog makeSureBigEnough applyLog doubleIt applyLog tripleIt)

  }

}
