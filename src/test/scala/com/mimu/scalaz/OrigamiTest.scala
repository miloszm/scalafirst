package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 *
 *
 * http://eed3si9n.com/learning-scalaz/Origami+programming.html
 *
 */



class OrigamiTest {

  /**
   * dual of folding is unfolding
   */
  @Test
  def testUnfolding():Unit = {

    val u1 = DList.unfoldr(10, { (x: Int) => if (x == 0) none else (x, x - 1).some })
    println(u1.toList)

    val s1 = unfold(10) { (x) => if (x == 0) none else (x, x - 1).some }
    println(s1)
    println(s1.toList)
  }

  /**
   * selection sort
   */
  @Test
  def testSelectionSort(): Unit ={

    def minimumS[A: Order](stream: Stream[A]) = stream match {
      case x #:: xs => xs.foldLeft(x) {_ min _}
    }

    def deleteS[A: Equal](y: A, stream: Stream[A]): Stream[A] = (y, stream) match {
      case (_, Stream()) => Stream()
      case (y, x #:: xs) =>
        if (y === x) xs
        else x #:: deleteS(y, xs)
    }

    def delmin[A: Order](stream: Stream[A]): Option[(A, Stream[A])] = stream match {
      case Stream() => none
      case xs =>
        val y = minimumS(xs)
        (y, deleteS(y, xs)).some
    }

    def ssort[A: Order](stream: Stream[A]): Stream[A] = unfold(stream){delmin[A]}

    println(ssort(Stream(8,3,5,1,9,4,7,2)).toList)

  }


}
