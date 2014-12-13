package com.mimu.fpinscala

import org.junit.Test

/**
 * Created by mm.
 */
class chapter2 {

  def partial1[A,B,C](a:A, f: (A,B) => C): B => C =
    x => f(a,x)

  /**
   * 2.3
   */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a:A) => ((b:B) => f(a,b))

  /**
   * 2.4
   */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a:A,b:B) => f(a)(b)

  /**
   * 2.5
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))

  @Test
  def testChapter2(): Unit ={

    val f = (x:Int) => x/2
    val g = (x:Int) => x*x

    println((f compose g)(4))   // 4*4 = 16 and then 16/2  =>  8
    println((f andThen g)(4))   // 4/2 = 2  and then 2*2   =>  4

  }

}
