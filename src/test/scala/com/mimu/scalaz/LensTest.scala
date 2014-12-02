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
 * https://www.youtube.com/watch?v=efv0SQNde5Q - Lenses A Functional Imperative - Edward Kmett
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
  def mod(f: B => B, a:AHolder): AHolder = set(f(get(a)),a)
  def andThen[C](l:MLens[B,C]):MLens[AHolder,C] = MLens[AHolder,C](
    g = ((a:AHolder) => l.get(get(a))),
    s = ((a:AHolder,c:C) => mod((b => l.set(c,b)), a))
  )
  def compose[C](that: MLens[C,AHolder]) = that andThen this
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

  /**
   * fst and snd
   */
  @Test
  def testLensFirstAndSecond():Unit = {

    type MPair[A,B] = (A,B)

    def fst[A,B]:MLens[MPair[A,B],A] = MLens[MPair[A,B],A](
      g = (p => p._1),
      s = ((p:MPair[A,B],a:A) => (a, p._2))
    )

    def snd[A,B]:MLens[MPair[A,B],B] = MLens[MPair[A,B],B](
      g = (p => p._2),
      s = ((p:MPair[A,B],b:B) => (p._1, b))
    )

    val foo:MPair[Int,Int] = (1, 2)

    println(fst.set(4, foo))

    val bar:MLens[MPair[Int,Int], Int] = snd.asInstanceOf[MLens[MPair[Int,Int],Int]]

    println(bar.set(5, foo))

  }

  /**
   * lens set example
   */
  @Test
  def testLensAndSet(): Unit ={
    def contains[K](k:K) =
       MLens[Set[K],Boolean](_ contains k, {
         case (s:Set[K], true) => s + k
         case (s:Set[K], false) => s - k
       })

    val foo = Set(1,2,3)
    println(contains(2).get(foo))
    println(contains(4).set(true, foo))
    println(contains(3).set(false, foo))

  }

  /**
   * check laws of lens
   */
  def checkLawsOfLens[AHolder,B](aHolder:AHolder, b:B, l:MLens[AHolder,B], c:B): Unit ={
    println(l.get(l.set(b, aHolder)) == b)
    println(l.set(c,l.set(b, aHolder)) == l.set(c, aHolder))
    println(l.set(l.get(aHolder),aHolder) == aHolder)
    assertTrue(l.get(l.set(b, aHolder)) == b) // I should get what I've set
    assertTrue(l.set(c,l.set(b, aHolder)) == l.set(c, aHolder)) // I've flushed out whatever was there and I am getting the new value
    assertTrue(l.set(l.get(aHolder),aHolder) == aHolder) // when I get and set immediately, I do not change anything
  }

}
