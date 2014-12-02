package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * lens
 *
 * http://scalathon.org/2012/presentations/lenses.pdf
 * https://www.youtube.com/watch?v=efv0SQNde5Q
 *
 */


/**
 * setting AHolder to B
 */
case class MLens[AHolder, B](
                             g: AHolder => B,
                             s: (AHolder, B) => AHolder
                             ){
  def get(a: AHolder): B = g(a)
  def set(b:B, a:AHolder): AHolder = s(a,b) // build a new holder this time with a new b in it
}


class LensTest {

  /**
   * lens
   */
  @Test
  def testLens():Unit = {

    case class PointX (x:Double,y:Double)

    val l = MLens[PointX,Double]( _.x, (obj, value) => obj.copy(x = value))
    checkLawsOfLens(PointX(1.0, 1.0), 2.0, l, 3.0)

    val ll = MLens[List[Int], Int]( _.head, (obj:List[Int], x:Int) => List(x) )
    checkLawsOfLens(List(3), 4, ll, 5)

  }

  def checkLawsOfLens[AHolder,B](aHolder:AHolder, b:B, l:MLens[AHolder,B], c:B): Unit ={
    println(l.get(l.set(b, aHolder)) == b)
    println(l.set(c,l.set(b, aHolder)) == l.set(c, aHolder))
    println(l.set(l.get(aHolder),aHolder) == aHolder)
    assertTrue(l.get(l.set(b, aHolder)) == b)
    assertTrue(l.set(c,l.set(b, aHolder)) == l.set(c, aHolder))
    assertTrue(l.set(l.get(aHolder),aHolder) == aHolder)
  }

}
